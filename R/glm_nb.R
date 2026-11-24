#' Fit a negative binomial GLM to a cv_set
#'
#' @inheritParams fit_ranger
#' @returns dataframe of prediction intervals
#' @export
fit_glm_nb <- function(cv_set, y_var, id_vars, pred_vars,
                       return_model = FALSE,
                       quantile_levels = c(0.01,0.025, seq(0.05,0.95, by = 0.05), 0.975, 0.99)){

  #debugging stuff
  # pred_vars <- c("rain_mm", "temp_c", "month_season", "orgUnit")
  # id_vars <- c("orgUnit", "date")
  # y_var = "n_case"
  # quantile_levels = c(0.01,0.025, seq(0.05,0.95, by = 0.05), 0.975, 0.99)

  #ensure that orgUnit and month of year are always included as predictor variables
  pred_vars <- unique(c("orgUnit", "month_season", pred_vars))

  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars),
                             remove_NA = TRUE)
  this_analysis <- cv_clean$analysis
  this_assess <- cv_clean$assess

  both_datasets <- dplyr::bind_rows(this_analysis, this_assess)

  #check that pred_vars are not static or collinear (rank deficient)
  kept_vars <- screen_glm_vars(train_df = this_analysis, pred_vars = pred_vars)
  if(length(kept_vars) < length(pred_vars)){
    warning(paste("Static or collinear variable found in GLM model:",
                  paste(setdiff(pred_vars, kept_vars), collapse = ", "),
                  ". Fitting without these variables.\n"))
    pred_vars <- kept_vars
  }

  fit <- retry_glm_nb(train_df = this_analysis, pred_vars = pred_vars)
  if(is.null(fit)){
    cli::cli_alert_warning("GLM cannot be fit. No predictions returned.")
    if(return_model){
      return(list("model" = NULL,
                  "predictions" = NULL))
    } else return(NULL)
  }
  glm_mod <- fit$model

  # Estimate prediction intervals via analytic method
  mod_pi <- as.data.frame(calc_pi_nb_analytic(df = both_datasets, fit = glm_mod,
                                              quantile_levels = quantile_levels)) |>
    cbind(dplyr::select(both_datasets, dplyr::all_of(c(id_vars, "y_obs", "dataset")))) |>
    tidyr::pivot_longer(cols = dplyr::all_of(dplyr::contains("quant_")),
                        names_to = "quant_long", values_to = "predicted") |>
    dplyr::mutate(quantile_level = as.numeric(gsub("quant_","", .data$quant_long)))
  colnames(mod_pi)[colnames(mod_pi) == "y_obs"] <- "observed"

  if(return_model){
    return(list("model" = glm_mod,
                "predictions" = mod_pi))
  } else return(mod_pi)

}



#' Internal function to drop predictors that make the GLM design matrix rank deficient
#'
#' Checks ahead of time for static or collinear predictors via a QR decomposition
#'   of the model matrix, which is much faster than refitting. Variables are added
#'   one at a time in the order of `pred_vars` and a variable is dropped if it does not
#'   add its full number of columns to the rank of the model matrix (this also catches
#'   interactions and factors). Variables in `fixed_vars` are never dropped.
#' @param train_df data.frame of data used to train the model
#' @param pred_vars vector of character names of predictor variables
#' @param fixed_vars vector of character names of variables that are always retained.
#'   Default = c("orgUnit", "month_season")
#' @param tol tolerance for the rank determination in [base::qr()]
#' @return vector of character names of the retained variables
screen_glm_vars <- function(train_df, pred_vars,
                            fixed_vars = c("orgUnit", "month_season"),
                            tol = 1e-7){
  fixed <- intersect(fixed_vars, pred_vars)
  candidates <- setdiff(pred_vars, fixed)

  #returns NULL if the model matrix cannot be built (e.g. factor with one level)
  mm_rank <- function(vars){
    X <- tryCatch(stats::model.matrix(stats::reformulate(if(length(vars)>0) vars else "1"),
                                      data = train_df),
                  error = function(e) NULL)
    if(is.null(X)) return(NULL)
    c("rank" = qr(X, tol = tol)$rank, "ncol" = ncol(X), "nrow" = nrow(X))
  }

  base <- mm_rank(fixed)
  #problems with the fixed variables are left for retry_glm_nb to handle
  if(is.null(base)) return(pred_vars)

  kept <- fixed
  for(v in candidates){
    new <- mm_rank(c(kept, v))
    if(!is.null(new) && new["nrow"] > new["ncol"] &&
       (new["rank"] - base["rank"]) == (new["ncol"] - base["ncol"])){
      kept <- c(kept, v)
      base <- new
    }
  }
  return(kept)
}

#' Internal function to iteratively fit a negative binomial GLM
#'
#' This function fits a negative binomial GLM via MASS::glm.nb(). It starts with all
#'   of the predictor variables and if the model errors or does not converge, it
#'   removes the last one in the list and tries to refit. Non-convergence is detected
#'   from the warnings thrown by glm.nb ("iteration limit reached", "alternation limit
#'   reached" and "algorithm did not converge"). It repeats this process until a model
#'   is successfully fit. If only the fixed variables are left, no model is
#'   fit and NULL is returned.
#' @param train_df data.frame of data used to train the model
#' @param pred_vars vector of character names of predictor variables. Variables
#'   in `fixed_vars` are moved to the front so that they are dropped last.
#' @param fixed_vars vector of character names of variables that are never
#'   dropped. Default = c("orgUnit", "month_season")
#' @return a list containing the fit model (`model`) and the variables it was fit
#'   with (`vars`), or NULL if no model could be fit.
retry_glm_nb <- function(train_df, pred_vars,
                         fixed_vars = c("orgUnit", "month_season")){
  new_pred_vars <- unique(c(fixed_vars, pred_vars))
  convergence_warnings <- "iteration limit reached|alternation limit reached|did not converge"

  repeat{
    fit_err <- NULL
    glm_mod <- tryCatch({
      mod <- withCallingHandlers(
        MASS::glm.nb(stats::reformulate(termlabels = new_pred_vars, response = "y_obs"),
                     data = train_df),
        warning = function(w){
          #convergence warnings are treated as errors, others are passed on
          if(grepl(convergence_warnings, conditionMessage(w))){
            stop(conditionMessage(w), call. = FALSE)
          }
        })
      if(isFALSE(mod$converged)) stop("glm.nb did not converge", call. = FALSE)
      mod
    }, error = function(e){
      fit_err <<- conditionMessage(e)
      NULL
    })

    if(!is.null(glm_mod)){
      return(list("model" = glm_mod,
                  "vars" = new_pred_vars))
    }

    dropped <- new_pred_vars[length(new_pred_vars)]
    new_pred_vars <- new_pred_vars[-length(new_pred_vars)]

    if(length(new_pred_vars) <= length(fixed_vars)){
      cli::cli_alert_warning("Unable to fit GLM ({fit_err}) and no optional predictors remain.")
      return(NULL)
    }
    cli::cli_alert_warning("Unable to fit GLM ({fit_err}). Dropping {dropped} and refitting.")
  }
}

#' Estimate glm.nb prediction interval analytically
#'
#' Internal function to get prediction intervals from glm model
#'
#' @details
#' This is based on the `trending` package, as described here: https://cran.r-project.org/web/packages/trending/vignettes/prediction_intervals.html
#'
#' @param df dataframe to get predictions on
#' @param fit model fit via MASS::glm.nb
#' @param quantile_levels quantiles that you want prediction intervals at
#' @returns matrix of prediction intervals for each row of df
calc_pi_nb_analytic <- function(df, fit,
                                quantile_levels = c(0.01,0.025, seq(0.05,0.95, by = 0.05), 0.975, 0.99)){
  #first calculate ci parametrically based on ciTools `parametric_ci_negbin`
  #then use those to estimate pi
  out <- stats::predict(fit, df, se.fit = TRUE, type = "link")
  crit_vals <- stats::qt(p = quantile_levels, df = fit$df.residual)
  inverselink <- fit$family$linkinv
  theta <- fit$theta
  ci_mat <- matrix(0, ncol = length(crit_vals), nrow = nrow(df))
  pi_mat <- matrix(0, ncol = length(crit_vals), nrow = nrow(df))
  for(i in 1:length(crit_vals)){
    ci_mat[,i] <- inverselink(out$fit + crit_vals[i] *out$se.fit)
    pi_mat[,i] <- stats::qnbinom(p = quantile_levels[i], mu = ci_mat[,i], size = theta)
    pi_mat[,i] <- ifelse(is.nan(pi_mat[,i]),0,pi_mat[,i])
  }
  colnames(pi_mat) <- paste0("quant_", quantile_levels)
  return(pi_mat)
}

#' Estimate variable importance and counter-factual plots of a glm.nb model
#'
#' @inheritParams fit_glm_nb
#' @param var_scales data.frame containing centering and scaling parameters for variables
#' @param constant_org which orgUnit to use for counterfactual plots. If NULL (default), chooses one
#' at random
#' @param constant_date whcih date to use for counterfactual plots. If NULL (default), is set to median of
#' analysis data.
#' @param nsim number of simulations to use in variable importance permutation. Default = 50.
#' @returns list containing variable importance scores and a list of dataframes
#'   containing data for counterfactual plots with each element corresponding to a variable
#' @export
inv_variables_glm_nb <- function(cv_set, y_var, id_vars, pred_vars, var_scales,
                                 constant_org = NULL, constant_date = NULL,
                                 nsim = 50){

  #ensure that orgUnit and month of year are always included as predictor variables
  pred_vars <- unique(c("orgUnit", "month_season", pred_vars))

  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars),
                             remove_NA = TRUE)
  this_analysis <- cv_clean$analysis
  if(is.null(constant_org)){
    constant_org <- as.character(sample(this_analysis$orgUnit,1))
  }
  if(is.null(constant_date)){
    constant_date <- as.Date(paste0(substr(median(this_analysis$date),1,8), "01"))
  }


  #---- create counterfactual dataframes -----
  counter_vars <- unique(unlist(strsplit(pred_vars, "\\*")))

  counter_one <- this_analysis |>
    dplyr::filter(.data$orgUnit == constant_org, .data$date == constant_date) |>
    dplyr::mutate(y_obs = NA)

  counter_data <- purrr::map(1:length(counter_vars), function(i){
    this_var_name <- counter_vars[i]
    #create a grid to fit
    var_class <- class(this_analysis[[this_var_name]])
    if(var_class == "numeric"){
      var_range <- range(this_analysis[this_var_name], na.rm = TRUE)
      var_grid <- seq(var_range[1], var_range[2], length.out = 50)
      if(nrow(unique(this_analysis[this_var_name]))<10){
        var_grid <- unique(this_analysis[[this_var_name]])
      }
    } else {
      var_grid <- unique(this_analysis[[this_var_name]])
    }
    this_counter <- counter_one[rep(1, length(var_grid)),]
    this_counter[this_var_name] <- var_grid
    this_counter$counter_variable <- this_var_name
    return(this_counter)
  }) |> dplyr::bind_rows()

  #create formula based on pred_vars
  glm_formula <- stats::reformulate(termlabels = pred_vars,
                             response = "y_obs")

  glm_mod <- MASS::glm.nb(glm_formula,
                          data = this_analysis)

  # ---- variable importance -------
  pfun <- function(object, newdata) stats::predict(object, newdata)
  vi_glm <- vip::vi(glm_mod, method = "permute", target = "y_obs", nsim = nsim,
                    metric = "mae", pred_wrapper = pfun,
                    train = this_analysis[,c("y_obs", counter_vars)]) |>
    dplyr::filter(.data$Variable %in% counter_vars) |>
    dplyr::mutate(Importance = ifelse(.data$Importance<0, 0, .data$Importance)) |>
    dplyr::mutate(Importance = .data$Importance/sum(.data$Importance))
  colnames(vi_glm) <- tolower(colnames(vi_glm))

  # ---- create counterfactual data -----
  counter_preds <- cbind(counter_data,
                         "pred" = stats::predict(glm_mod, newdata = counter_data,
                                          type = "response"))
  counter_out <- purrr::map(1:length(counter_vars), function(i,...){
    pdp_df <- counter_preds[counter_preds$counter_variable == counter_vars[i],]
    pdp_df <- pdp_df[,c("counter_variable", counter_vars[i], "pred")]
    colnames(pdp_df) <- c("variable", "var_valuesc", "yhat")
    pdp_df <- dplyr::distinct(pdp_df)
    if(stringr::str_sub(counter_vars[i], -2,-1)=="sc") {
      pdp_df <- pdp_df |>
        dplyr::left_join(var_scales, by = "variable") |>
        dplyr::mutate(var_value = .data$var_valuesc*.data$scale + .data$center) |>
        dplyr::select(-dplyr::all_of(c("scale", "center")))
    } else {
      pdp_df <- pdp_df |>
        dplyr::mutate(var_value = .data$var_valuesc)
    }
  })

  return(list("varImp" = vi_glm,
              "counter_data" = counter_out))
}
