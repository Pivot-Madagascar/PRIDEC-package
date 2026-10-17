#' Fit an INLA model to one cv_set and get prediction intervals
#' @inheritParams fit_ranger
#' @param hyper_priors priors for spatial and temporal random effects
#' @param reff_var string or formula for random effects
#' @param return_model whether or not to return the INLA model in addition to predictions
#' @param sample_pi whether to estimate the prediction intervals by sampling the
#'   posterior. FALSE uses a trick to put all error into an iid random effect. Default = TRUE
#' @param W_orgUnit graph of orgUnit to use in spatial structure. If NULL (default), will not fit a spatial structure.
#' @param verbose whether to run the INLA model verbosely. Default = FALSE
#' @param n_cores number of cores to use (integer). Defaults to number of available cores/2
#'
#' @returns dataframe of predictions intervals
#' @export
fit_inla <- function(cv_set, y_var, pred_vars, id_vars, reff_var = NULL,
                     hyper_priors = INLA_HYPER_DEFAULT,
                     return_model = FALSE,
                     quantile_levels = c(0.01,0.025, seq(0.05,0.95, by = 0.05), 0.975, 0.99),
                     sample_pi = INLA_SAMPLEPI_DEFAULT,
                     W_orgUnit = NULL,
                     verbose = TRUE,
                     n_cores = NULL){

  # ------ set up data ------
  both_datasets <- create_inla_data_basic(cv_set = cv_set, y_var = y_var, pred_vars = pred_vars, id_vars = id_vars)

  #------run inla-------------------------------

  inla_mod <- run_inla_config(data = both_datasets, pred_vars = pred_vars,
                              verbose = verbose, family = 'zeroinflatednbinomial1',
                              n_cores = n_cores, hyper_priors = hyper_priors, W_orgUnit = W_orgUnit)

  if(is.null(inla_mod)) {
    cli::cli_alert_warning("Removing INLA from ensemble due to convergence issues.")
    return(NULL)
  }

  # ----- prediction intervals -------------------
  id_dataframe <- both_datasets[,c("orgUnit", "date", "dataset", "observed")]

  if(sample_pi){
    post_pis <- INLA::inla.posterior.sample(1000, inla_mod)
    pi_samples <- INLA::inla.posterior.sample.eval(fun = "Predictor", samples = post_pis)

    preds_pi <- purrr::map(1:nrow(pi_samples),
                    function(x) get_inla_pi_sample(pi_samples[x, ],
                                                   quantile_levels, id_df = id_dataframe[x,])
    ) |>
      dplyr::bind_rows()
  } else {
    preds_pi <- purrr::map(1:nrow(id_dataframe),
                    function(x) get_inla_pi(inla_mod$marginals.fitted.values[[x]],
                                            quantile_levels, id_df = id_dataframe[x,])
    ) |>
      dplyr::bind_rows()
  }

  if(return_model){
    return(list("model" = inla_mod,
                "predictions" = preds_pi))
  } else return(preds_pi)

}

#' Calculate variable importance in INLA via simulation
#'
#' @inheritParams fit_inla
#' @param nsims number of simulations to run. Default = 1
#' @param seed a seed to use for reproducability. Default = 8675309
#' @returns dataframe of variable importance scores
calc_inla_vi <- function(cv_set, y_var, pred_vars, id_vars,
                         reff_var = NULL,
                         hyper_priors = INLA_HYPER_DEFAULT,
                         W_orgUnit,
                         seed = 8675309,
                         nsims = 1,
                         n_cores = NULL){
  set.seed(seed)
  both_datasets <- create_inla_data_vi(cv_set = cv_set, y_var = y_var, pred_vars = pred_vars,
                                       id_vars = id_vars, nsims = nsims)

  #---- run inla -------------------
  inla_mod <- run_inla_config(data = both_datasets, pred_vars = pred_vars,
                              verbose = FALSE, family = 'zeroinflatednbinomial1',
                              n_cores = n_cores, hyper_priors = hyper_priors, W_orgUnit = W_orgUnit)

  if(is.null(inla_mod)) {
    cli::cli_alert_warning("Cannot fit INLA model due to convergence issues.")
    return(NULL)
  }

  #---- calculate importance via performance on permuted data ------
  permute_out <- both_datasets[,c(id_vars, "y_obs", "observed",
                        "perm_var", "nsim", "dataset")]
  permute_out$predicted <- inla_mod$summary.fitted.values$`0.5quant`
  permute_out$quant_long <- "quant_0.5"
  permute_out$quantile_level <- 0.5
  permute_out$dataset <- paste(permute_out$perm_var, permute_out$nsim, sep = "-sim")

  #estimate median absolute error
  var_imp <- eval_performance(permute_out)[,c("dataset", "med_ae")]
  var_imp$original <- var_imp$med_ae[var_imp$dataset == "original-sim0"]
  var_imp <- var_imp |>
    tidyr::separate(.data$dataset, into = c("perm_var", "nsim"), sep = "-sim") |>
    dplyr::mutate(err_diff = .data$med_ae - .data$original) |>
    dplyr::mutate(err_diff = ifelse(.data$err_diff<0,0, .data$err_diff)) |>
    dplyr::summarise(err_diff = mean(.data$err_diff, na.rm = TRUE),
              .by = "perm_var") |>
    dplyr::mutate(imp = .data$err_diff/sum(.data$err_diff)) |>
    dplyr::filter(.data$perm_var != "original") |>
    dplyr::select(dplyr::all_of(c("variable" = "perm_var", "importance" = "imp")))

  return(var_imp)

}

#' Create counterfactual data of variables from INLA model
#' @inheritParams fit_inla
#' @param var_scales data.frame of center and scale factors used in variable scaling
#' @param constant_org orgUnit to use in counterfactual plots
#' @param constant_date date to use in counterfactual plots
#' @returns list of dataframes of counter factual data. Each element corresponds to one predictor variable
create_counterfactual_inla <- function(cv_set, y_var, pred_vars, id_vars,
                                       reff_var = NULL,
                                       hyper_priors = INLA_HYPER_DEFAULT,
                                       W_orgUnit, var_scales,
                                       constant_org, constant_date,
                                       n_cores = NULL){

  both_datasets <- create_inla_data_counterfactual(cv_set = cv_set, y_var = y_var, pred_vars = pred_vars,
                                                   id_vars = id_vars, constant_org = constant_org, constant_date = constant_date)

  #------run inla-------------------------------
  inla_mod <- run_inla_config(data = both_datasets, pred_vars = pred_vars, family = 'zeroinflatednbinomial1',
                              n_cores = n_cores, hyper_priors = hyper_priors, W_orgUnit = W_orgUnit)

  if(is.null(inla_mod)) {
    cli::cli_alert_warning("Cannot fit INLA model due to convergence issues.")
    return(NULL)
  }
  #----transform into pdp dataframe --------

  counter_preds <- both_datasets
  counter_preds$pred <- inla_mod$summary.fitted.values$`0.5quant`
  counter_preds <- subset(counter_preds, counter_preds$dataset == "counter")
  counter_vars <- c("org_ID", "month_season", "month_num", pred_vars)

  counter_out <- purrr::map(1:length(counter_vars), function(i,...){
    pdp_df <- dplyr::filter(counter_preds, .data$counter_variable == counter_vars[i]) |>
      dplyr::rename(var_valuesc = counter_vars[i]) |>
      dplyr::select(dplyr::all_of(c("variable" = "counter_variable", "var_valuesc", "yhat" = "pred"))) |>
      dplyr::distinct()
    if(stringr::str_sub(counter_vars[i], -2,-1)=="sc") {
      pdp_df <- pdp_df |>
        dplyr::left_join(var_scales, by = "variable") |>
        dplyr::mutate(var_value = .data$var_valuesc*.data$scale + .data$center) |>
        dplyr::select(-all_of(c("scale", "center")))
    } else {
      pdp_df <- pdp_df |>
        dplyr::mutate(var_value = .data$var_valuesc)
    }
    return(pdp_df)
  })
  return(counter_out)
}

#' Estimate variable importance and partial dependence plots of a  INLA model
#' @inheritParams fit_inla
#' @inheritParams create_counterfactual_inla
#' @inheritParams calc_inla_vi
#' @param var_scales data.frame containing centering and scaling parameters for variables
#'
#' @returns list containing variable importance scores and a list of dataframes
#'   containing data for pdp plots with each element corresponding to a variable
#' @export
inv_variables_inla <- function(cv_set, y_var, pred_vars, reff_var = NULL, id_vars,
                               hyper_priors = INLA_HYPER_DEFAULT,
                               W_orgUnit, var_scales,
                               constant_org, constant_date,
                               seed = 8675309,
                               nsims = 1,
                               n_cores = NULL){

  var_imp <- calc_inla_vi(cv_set = cv_set,
               y_var = y_var,
               pred_vars = pred_vars,
               id_vars = id_vars,
               reff_var = reff_var,
               hyper_priors = hyper_priors,
               W_orgUnit = W_orgUnit,
               seed = seed,
               nsims = nsims,
               n_cores = n_cores)

  counter_list <- create_counterfactual_inla(cv_set = cv_set,
                                             y_var = y_var,
                                             pred_vars = pred_vars,
                                             id_vars = id_vars,
                                             reff_var = reff_var,
                                             hyper_priors = hyper_priors,
                                             W_orgUnit = W_orgUnit,
                                             constant_org = constant_org,
                                             constant_date = constant_date,
                                             var_scales = var_scales,
                                             n_cores = n_cores)

  return(list("var_imp" = var_imp,
              "counter_data" = counter_list))

}

create_inla_setup <- function(hyper_priors = INLA_HYPER_DEFAULT, W_orgUnit=NULL){

  if(is.null(hyper_priors)){
    hyper_priors <- list("prec.spatial" = c(1, 0.01),
                         "phi.spatial" = c(0.5,0.5),
                         "prec.timerw1" = c(1,0.01))
  }

  format_param <- function(p) paste0("c(", paste(p, collapse = ", "), ")")

  prior_sp <- list(
    #structured spatial variance, c(u, alpha): P(sd > u) = alpha
    prec = list(
      prior = "pc.prec",
      param = hyper_priors$prec.spatial
    ),
    # unsctructured spatial variance, c(u, alpha): P(phi < u) = alpha
    phi = list(
      prior = "pc",
      param = hyper_priors$phi.spatial
    )
  )

  #following recommendation in documentation for rw1 for poisson (higher param[1] = weaker), c(1,0.01)
  prior_time <- list(
    #precision
    prec = list(
      prior = "pc.prec",
      param = hyper_priors$prec.timerw1
    )
  )
  reff_var <- sprintf(
    "f(month_season, model = 'rw1', cyclic = T, group = org_ID, control.group = list(model = 'iid'), scale.model = TRUE, hyper = list(prec = list(prior = '%s', param = %s)))",
    prior_time$prec$prior, format_param(prior_time$prec$param)
  )
  if(!(is.null(W_orgUnit))){
    reff_space <- sprintf(
      "f(org_ID, model = 'bym2', graph = W_orgUnit, scale.model = TRUE, hyper = list(prec = list(prior = '%s', param = %s), phi = list(prior = '%s', param = %s)))",
      prior_sp$prec$prior, format_param(prior_sp$prec$param),
      prior_sp$phi$prior,  format_param(prior_sp$phi$param)
    )
    reff_var <- c(reff_var, reff_space)
  }

  pi_var <- c("f(idx, model = 'iid', hyper = list(prec = list(param = c(1,0.01))))")

  return(list(prior_sp = prior_sp,
              prior_time = prior_time,
              reff_var = reff_var,
              pi_var = pi_var))

}

#' Sample from fixed-effect posteriors
#' @param inla_model INLA model object
#' @param n_samp number of samples to take from posterior
sample_fixed_posteriors <- function(inla_model, n_samp = 1e4){

  fixed_posts <- purrr::map(1:length(inla_model$marginals.fixed), function(x) {
    INLA::inla.rmarginal(n_samp, inla_model$marginals.fixed[[x]])
  })
  names(fixed_posts) <- names(inla_model$marginals.fixed)
  fixed_posts_df <- dplyr::bind_rows(fixed_posts) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "var_name", values_to = "sample")

  return(fixed_posts_df)
}

#' Function to calculate and format posterior prediction distributions from INLA via posterior sampling
#' @param inla_post_sample samples for one predictor (often output of inla.posterior.sample.eval)
#' @param quantile_levels quantiles that you want to estimate interval at
#' @param id_df a one row dataframe containing the id variables and true_value for that observation
get_inla_pi_sample <- function(inla_post_sample, quantile_levels, id_df){

  pis <- round(exp(stats::quantile(inla_post_sample, probs = quantile_levels)))

  pred_pi <- data.frame(predicted = pis,
                        quantile_level = quantile_levels
  ) |>
    mutate(quant_long = paste0("quant_", as.character(.data$quantile_level)))

  pred_pi <- cbind(id_df[rep(1, nrow(pred_pi)),], pred_pi)
  rownames(pred_pi) <- NULL

  return(pred_pi)

}

#' Function to calculate and format posterior prediction distributions from INLA using random error trick
#'
#' One way to calculate the PI for an INLA model with the appropriate error is to fit a random error
#' term to each observation. This takes the output of that to estimate PIs.
#'
#' @param inla_marginal output of `marginals.fitted.values` from INLA model for one observation
#' @param quantile_levels quantiles that you want to estimate interval at
#' @param id_df a one row dataframe containing the id variables and true_value for that observation
get_inla_pi <- function(inla_marginal, quantile_levels, id_df){

  pis <- round(INLA::inla.qmarginal(quantile_levels, inla_marginal))

  pred_pi <- data.frame(predicted = pis,
                        quantile_level = quantile_levels
  ) |>
    mutate(quant_long = paste0("quant_", as.character(.data$quantile_level)))

  pred_pi <- cbind(id_df[rep(1, nrow(pred_pi)),], pred_pi)
  rownames(pred_pi) <- NULL

  return(pred_pi)
}


#' Configuration for an INLA model
#' An internal function to run a configured INLA model with a limit on the time and number of times to try to fit
#' If a model cannot be fit within those tries, it returns NULL
#'
#' @param formula formula in y ~x format for model
#' @param data data to be used in model
#' @param family family of model to fit. Default = "zeroinflatednbinomial1"
#' @param config whetehr or not to use config for control.compute step. Default = TRUE
#' @param verbose whether fitting should be verbose. Default = TRUE
#' @param n_cores number of cores to use. If null uses available cores/2
run_inla_config <- function(formula, data, pred_vars, family = "zeroinflatednbinomial1",
                            config = T, verbose = T, n_cores = NULL, hyper_priors = INLA_HYPER_DEFAULT, W_orgUnit = NULL,
                            reff_var = NULL, sample_pi = INLA_SAMPLEPI_DEFAULT,
                            max_time = INLA_MAX_TIME_DEFAULT, max_tries = INLA_MAX_TRY_DEFAULT){
  if(is.null(n_cores)){
    n_cores <- pmax(ceiling(parallel::detectCores()/2),1)
  }
  #diagnose for problems
  diagnosis <- diagnose_inla_data(data = data, W_orgUnit = W_orgUnit, pred_vars = pred_vars)
  #these problems make the precision matrix singular, so don't bother fitting
  fatal <- c(names(diagnosis[[1]])[names(diagnosis[[1]]) %in% c("missing_from_graph",
                                                                  "design_matrix_rank_deficient")],
             names(diagnosis[[2]]))
  if(length(fatal) > 0){
    cli::cli_alert_warning("Cannot fit INLA due to data problems: {paste(unique(fatal), collapse = ', ')}")
    return(NULL)
  }

  #------check collinearity ---------------------------------
  collinear <- check_collinearity(data[pred_vars])
  if(collinear){
    cli::cli_alert_warning("Cannot fit INLA due to collinearity between predictor variables.")
    return(NULL)
  }

  #---- check for holes in W_orgUnit ----------------------
  if(!is.null(W_orgUnit)){
    g <- INLA::inla.read.graph(W_orgUnit)
    if(g$cc$n >1){
      cli::cli_alert_warning("INLA graph contains islands. Cannot fit.")
      return(NULL)
    }
  }
  #--------create priors---------------------------------

  prior_setup <- create_inla_setup(hyper_priors, W_orgUnit)

  if(is.null(reff_var)){
    reff_var <- prior_setup$reff_var
  }
  if(sample_pi) prior_setup$pi_var <- NULL

  formula <- stats::reformulate(termlabels = c(reff_var, pred_vars, prior_setup$pi_var),
                                     response = "y_obs")


  for (i in seq_len(max_tries)) {
    model_fit <- tryCatch({
      callr::r(
        func = function(formula, data, family, config, verbose, n_cores, prior_setup, W_orgUnit){
          INLA::inla(formula = formula, data = data, family = family,
                                     control.inla = list(strategy = 'auto'),
                                     control.compute = list(dic = T, config = config,
                                                            waic = T, return.marginals.predictor = T),
                                     control.predictor = list(link = 1, compute = T),
                                     control.fixed = list(prec.intercept = 0.01),
                                     num.threads = n_cores,
                                     verbose = verbose)
        },
        args = list(formula = formula, data = data, family = family, config = config,
                    verbose = verbose, n_cores = n_cores,
                    prior_setup = prior_setup, W_orgUnit = W_orgUnit),
        timeout = max_time
      )
    }, error = function(e) {
      message("INLA fitting attempt ", i, " failed or timed out after ", max_time, "s : ", conditionMessage(e))
      NULL
    })
    if(!is.null(model_fit)){
      problem <- check_inla_fit(model_fit, config = config)
      if(is.null(problem)) return(model_fit)
      message("INLA fitting attempt ", i, " did not produce a usable model: ", problem)
    }
  }
  message("All INLA fit attempts hung or failed.")
  return(NULL)
}

#' Check that a fitted INLA object is actually usable
#'
#' When the C program hits a non positive-definite matrix, `inla()` can return
#' an object without raising an R error. That object is missing pieces
#' (e.g. `misc$configs`) and breaks later, e.g. in `inla.posterior.sample()`.
#' @param model_fit output of `INLA::inla()`
#' @param config whether the model was fit with `control.compute(config = TRUE)`
#' @returns NULL if the model looks fine, otherwise a string describing the problem
#' @noRd
check_inla_fit <- function(model_fit, config = TRUE){
  if(!is.list(model_fit)) return("fit is not a model object")
  if(!isTRUE(model_fit$ok)) return("INLA reported the run as not ok")
  fv <- model_fit$summary.fitted.values
  if(is.null(fv) || nrow(fv) == 0 || all(is.na(fv$`0.5quant`))) return("no fitted values")
  if(config){
    cfg <- model_fit$misc$configs
    if(is.null(cfg) || !is.list(cfg) || is.null(cfg$nconfig) || cfg$nconfig < 1) {
      return("no posterior configurations (needed for sampling)")
    }
  }
  NULL
}

create_inla_data_basic <- function(cv_set, y_var, pred_vars, id_vars){


  # ------ set up data ------
  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars,
                                                                  "org_ID",
                                                                  "month_season", "month_num"),
                             remove_NA = FALSE)
  this_analysis <- cv_clean$analysis
  this_analysis$observed <- this_analysis$y_obs

  this_assess <- cv_clean$assess
  #will predict on things that are NA
  this_assess$observed <- this_assess$y_obs
  this_assess$y_obs <- as.numeric(NA)

  both_datasets <- dplyr::bind_rows(this_analysis, this_assess)
  both_datasets$idx <- 1:nrow(both_datasets)

  return(both_datasets)
}

create_inla_data_vi <- function(cv_set, y_var, pred_vars, id_vars, nsims){
  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars, "org_ID",
                                                                  "month_season", "month_num"),
                             remove_NA = FALSE)
  this_analysis <- cv_clean$analysis
  this_analysis$observed <- this_analysis$y_obs
  this_analysis$nsim <- 0
  this_analysis$perm_var = "original"

  #---- create variable importance data ---------
  permute_vars <- pred_vars


  permute_data <- purrr::map(1:length(permute_vars), function(var_i,...){
    this_var_permute <- purrr::map(1:nsims, function(sim_i,...){
      this_permute_df <- this_analysis |>
        dplyr::mutate(perm_var = permute_vars[[var_i]],
                      nsim = sim_i)
      this_permute_df[permute_vars[[var_i]]] <- sample(this_permute_df[[permute_vars[[var_i]]]],
                                                       size = nrow(this_permute_df),
                                                       replace = FALSE)
      return(this_permute_df)
    }) |>
      dplyr::bind_rows() #end nsims simulation of permutation for one variable

  }) |>
    dplyr::bind_rows() #end map over all pred_vars

  permute_data <- permute_data |>
    dplyr::mutate(y_obs = NA) |>
    dplyr::select(dplyr::all_of(c("y_obs", "observed", id_vars,
                                  "org_ID",
                                  "month_season", "month_num",
                                  pred_vars, "nsim", "perm_var"))) |>
    dplyr::mutate(dataset = "varImp")

  both_datasets <- dplyr::bind_rows(this_analysis, permute_data)
  return(both_datasets)
}

create_inla_data_counterfactual <- function(cv_set, y_var, pred_vars, id_vars, constant_org, constant_date){
  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars, "org_ID",
                                                                  "month_season", "month_num"),
                             remove_NA = TRUE)
  this_analysis <- cv_clean$analysis
  this_analysis$observed <- this_analysis$y_obs

  counter_one <- subset(this_analysis, this_analysis$orgUnit == constant_org & this_analysis$date == constant_date)
  counter_one$y_obs <- NA

  #throw error message if the constant_org and constant_date aren't working
  if(nrow(counter_one)!=1){
    cli::cli_abort(c(
      "The combination of {.var constant_org} and {.var constant_date} has an NA in the y_var column.",
      "Please supply new values for these constants."
    ))
  }

  #loop over pred_vars to create counter-data for each one
  counter_vars <- c("org_ID", "month_season", "month_num", pred_vars)

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

  both_datasets <- dplyr::bind_rows(mutate(this_analysis, dataset = "analysis"),
                                    mutate(counter_data, dataset = "counter"))
  return(both_datasets)
}

diagnose_inla_data <- function(data, W_orgUnit = NULL, pred_vars,
                               org_var = "org_ID", month_var = "month_season",
                               min_months_per_group = 3) {
  tol <- 1e-8
  issues <- list()

  # 1. org_IDs in data missing from the graph -> hard error in inla()
  if (!is.null(W_orgUnit)) {
    data_orgs  <- na.omit(unique(as.character(data[[org_var]])))
    graph_orgs <- rownames(W_orgUnit)
    missing_from_graph <- setdiff(data_orgs, graph_orgs)
    if (length(missing_from_graph) > 0) {
      issues$missing_from_graph <- missing_from_graph
      cli::cli_alert_danger("org_IDs in data but not in W_orgUnit: {paste(missing_from_graph, collapse=', ')}")
    }

    # 2. islands in the full graph
    g <- INLA::inla.read.graph(W_orgUnit)
    if (g$cc$n > 1) {
      issues$islands <- g$cc$n
      cli::cli_alert_danger("Graph has {g$cc$n} disconnected components (islands).")
    }

    # 3. rank / near-singularity check on the ICAR structure matrix actually
    #    used by bym2 -- catches near-degenerate substructure even without
    #    full islands, e.g. a node with only one active neighbour after
    #    subsetting, or numerically weak connectivity
    adj <- W_orgUnit != 0
    deg <- Matrix::rowSums(adj)
    Q_icar <- Matrix::Diagonal(x = deg) - adj
    ev <- tryCatch(
      eigen(as.matrix(Q_icar), symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) NA
    )
    if (!all(is.na(ev))) {
      n_near_zero <- sum(ev < 1e-8)
      # exactly 1 near-zero eigenvalue per connected component is expected
      # and handled internally by bym2's rank correction; MORE than that
      # signals real degeneracy
      if (n_near_zero > g$cc$n) {
        issues$icar_rank_deficient <- n_near_zero
        cli::cli_alert_danger(
          "ICAR structure matrix has {n_near_zero} near-zero eigenvalues, expected {g$cc$n}. Possible extra degeneracy."
        )
      }
    }

    # 4. any node with degree 0 (isolated within its own row, distinct from
    #    full multi-node islands above -- catches a lone singleton dropped
    #    out during subsetting)
    zero_degree <- graph_orgs[deg == 0]
    if (length(zero_degree) > 0) {
      issues$zero_degree_nodes <- zero_degree
      cli::cli_alert_warning("Nodes with zero neighbours: {paste(zero_degree, collapse=', ')}")
    }
  }

  # 5. per-org_ID temporal coverage for the cyclic grouped rw1 --
  #    thin coverage in THIS subset (not necessarily the full dataset)
  #    can make that group's block near-degenerate
  coverage <- tapply(data[[month_var]], data[[org_var]], function(x) length(unique(x)))
  thin_orgs <- names(coverage)[coverage < min_months_per_group]
  if (length(thin_orgs) > 0) {
    issues$thin_month_coverage <- coverage[thin_orgs]
    cli::cli_alert_warning(
      "org_IDs with < {min_months_per_group} distinct months in this subset: {paste(thin_orgs, collapse=', ')}"
    )
  }

  # 6. org_IDs with only 1 row total in this subset -- can't inform
  #    both the rw1xiid group block and contribute meaningfully
  n_per_org <- table(data[[org_var]])
  singleton_orgs <- names(n_per_org)[n_per_org == 1]
  if (length(singleton_orgs) > 0) {
    issues$singleton_orgs <- singleton_orgs
    cli::cli_alert_warning("org_IDs with only 1 observation in this subset: {paste(singleton_orgs, collapse=', ')}")
  }

  # 7. dropped/unused factor levels -- can silently change group indexing
  #    between runs on different subsets
  for (v in c(org_var, month_var)) {
    if (is.factor(data[[v]])) {
      unused <- setdiff(levels(data[[v]]), unique(as.character(data[[v]])))
      if (length(unused) > 0) {
        issues[[paste0("unused_levels_", v)]] <- unused
        cli::cli_alert_info("{v} has unused factor levels in this subset: {paste(unused, collapse=', ')}")
      }
    }
  }

  #--- predictor variables ------------------
  predvar_issues <- list()
  for (v in pred_vars) {
    x <- data[[v]]
    if (is.numeric(x)) {
      v_var <- stats::var(x, na.rm = TRUE)
      if (is.na(v_var) || v_var < tol) {
        predvar_issues[[v]] <- "zero or near-zero variance in this subset"
        cli::cli_alert_danger("Predictor '{v}' has near-zero variance in this subset (var = {signif(v_var, 4)}).")
      }
    } else {
      # factor/character: check for singleton or all-identical levels
      tab <- table(x)
      if (length(tab) < 2) {
        predvar_issues[[v]] <- "only one level present in this subset"
        cli::cli_alert_danger("Predictor '{v}' has only one level in this subset: {names(tab)}.")
      }
    }
  }

  # rank check on the full fixed-effects design matrix, catching
  # multicollinearity that a pairwise check could miss
  X <- stats::model.matrix(stats::reformulate(pred_vars), data = data)
  qr_X <- qr(X)
  if (qr_X$rank < ncol(X)) {
    deficient <- ncol(X) - qr_X$rank
    issues$design_matrix_rank_deficient <- deficient
    cli::cli_alert_danger("Fixed-effects design matrix is rank-deficient by {deficient} column(s) in this subset.")
  }

  if (length(issues) == 0) {
    cli::cli_alert_success("No structural issues detected in this data.")
  }

  if (length(predvar_issues) == 0) {
    cli::cli_alert_success("No predictor variable issues detected in this data.")
  }


  invisible(list(issues, predvar_issues))
}
