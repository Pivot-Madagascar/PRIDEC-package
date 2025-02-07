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

  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars),
                             remove_NA = TRUE)
  this_analysis <- cv_clean$analysis
  this_assess <- cv_clean$assess

  both_datasets <- dplyr::bind_rows(this_analysis, this_assess)

  #create formula based on pred_vars
  glm_formula <- stats::reformulate(termlabels = pred_vars,
                             response = "y_obs")

  glm_mod <- MASS::glm.nb(glm_formula,
                          data = this_analysis)

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
    dplyr::filter(.data$Variable %in% counter_vars)
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
