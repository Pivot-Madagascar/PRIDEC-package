#' Fit an INLA model to one cv_set and get prediction intervals
#' @inheritParams fit_ranger
#' @param hyper_priors priors for spatial and temporal random effects. Default: \code{INLA_HYPER_DEFAULT}
#' @param reff_var string or formula for random effects
#' @param return_model whether or not to return the INLA model in addition to predictions
#' @param sample_pi whether to estimate the prediction intervals by sampling the
#'   posterior. FALSE uses a trick to put all error into an iid random effect. Default = \code{INLA_SAMPLEPI_DEFAULT}
#' @param W_orgUnit graph of orgUnit to use in spatial structure. If NULL (default), will not fit a spatial structure.
#' @param verbose whether to run the INLA model verbosely. Default = TRUE
#' @param n_cores number of cores to use (integer). If NULL (default), uses available cores/2
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


  both_datasets <- create_inla_data_basic(cv_set = cv_set, y_var = y_var, pred_vars = pred_vars, id_vars = id_vars)

  inla_mod <- run_inla_config(data = both_datasets, pred_vars = pred_vars,
                              verbose = verbose, family = 'zeroinflatednbinomial1',
                              n_cores = n_cores, hyper_priors = hyper_priors, W_orgUnit = W_orgUnit)

  if(is.null(inla_mod)) {
    cli::cli_alert_warning("Removing INLA from ensemble due to convergence issues.")
    return(NULL)
  }

  # get PIs based on sampling regime
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

#' Configuration for an INLA model
#' An internal function to run a configured INLA model with a limit on the time and number of times to try to fit
#' If a model cannot be fit within those tries, it returns NULL
#'
#' @param formula formula in y ~x format for model
#' @param data data to be used in model
#' @param family family of model to fit. Default = "zeroinflatednbinomial1"
#' @param config whetehr or not to use config for control.compute step. Default = TRUE
#' @param verbose whether fitting should be verbose. Default = TRUE
#' @param n_cores number of cores to use. Defaults to ncores_default() if not provided.
run_inla_config <- function(formula, data, pred_vars, family = "zeroinflatednbinomial1",
                            config = T, verbose = T, n_cores = ncores_default(), hyper_priors = INLA_HYPER_DEFAULT, W_orgUnit = NULL,
                            reff_var = NULL, sample_pi = INLA_SAMPLEPI_DEFAULT,
                            max_time = INLA_MAX_TIME_DEFAULT, max_tries = INLA_MAX_TRY_DEFAULT){
  # diagnose data for problems
  diagnosis <- diagnose_inla_data(data = data, W_orgUnit = W_orgUnit, pred_vars = pred_vars)
  #these problems make the precision matrix singular, so don't bother fitting
  fatal <- c(names(diagnosis[[1]])[names(diagnosis[[1]]) %in% c("missing_from_graph",
                                                                "design_matrix_rank_deficient")],
             names(diagnosis[[2]]))
  if(length(fatal) > 0){
    cli::cli_alert_warning("Cannot fit INLA due to data problems: {paste(unique(fatal), collapse = ', ')}")
    return(NULL)
  }

  # create priors
  prior_setup <- create_inla_setup(hyper_priors, W_orgUnit)

  if(is.null(reff_var)){
    reff_var <- prior_setup$reff_var
  }
  if(sample_pi) prior_setup$pi_var <- NULL

  formula <- stats::reformulate(termlabels = c(reff_var, pred_vars, prior_setup$pi_var),
                                response = "y_obs")

  #fit model max_tries times

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

#' Set up priors and random effects for INLA model
#' @param hyper_priors Named list containing `prec.spatial`, `phi.spatial`, `prec.timerw1`. Defaults correspond to \code{INLA_HYPER_DEFAULT}
#' @param W_orgUnit graph of orgunits for INLA model
#' @returns list of priors and random effects
create_inla_setup <- function(hyper_priors = INLA_HYPER_DEFAULT, W_orgUnit=NULL){

  #backwards compabitbility for hyper_priors=NULL
  if(is.null(hyper_priors)) {
    hyper_priors = INLA_HYPER_DEFAULT
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



