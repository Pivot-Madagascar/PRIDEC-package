#' Fit an INLA model to one cv_set and get prediction intervals
#' @inheritParams fit_ranger
#' @param hyper_priors priors for spatial and temporal random effects
#' @param reff_var string or formula for random effects
#' @param return_model whether or not to return the INLA model in addition to predictions
#' @param sample_pi whether to estimate the prediction intervals by sampling the
#'   posterior. Default (FALSE) uses a trick to put all error into an iid random effect.
#' @param W_orgUnit graph of orgUnit to use in spatial structure
#'
fit_inla <- function(cv_set, y_var, pred_vars, id_vars, reff_var = NULL,
                     hyper_priors = list("prec.unstruct" = c(1, 5e-4),
                                         "prec.spatial" = c(1, 5e-4),
                                         "prec.timerw1" = c(1,0.01)),
                     return_model = FALSE,
                     quantile_levels = c(0.01,0.025, seq(0.05,0.95, by = 0.05), 0.975, 0.99),
                     sample_pi = FALSE,
                     W_orgUnit){

  prior_setup <- create_inla_setup(hyper_priors)

  if(is.null(reff_var)){
    reff_var <- prior_setup$reff_var
  }
  if(sample_pi) prior_setup$pi_var <- NULL

  # ------ set up data ------
  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars,
                                                                  "org_ID",
                                                                  "month_season", "month_num"),
                             remove_NA = TRUE)
  this_analysis <- cv_clean$analysis
  this_analysis$observed <- this_analysis$y_obs

  this_assess <- cv_clean$assess
  #will predict on things that are NA
  this_assess$observed <- this_assess$y_obs
  this_assess$y_obs <- as.numeric(NA)

  both_datasets <- dplyr::bind_rows(this_analysis, this_assess)
  both_datasets$idx <- 1:nrow(both_datasets)

  #------run inla-------------------------------
  formula_inla <- stats::reformulate(termlabels = c(reff_var, pred_vars, prior_setup$pi_var),
                              response = "y_obs")

  inla_mod <- run_inla_config(formula = formula_inla, data = both_datasets,
                              verbose = FALSE, family = 'zeroinflatednbinomial1')

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
calc_inla_vi <- function(cv_set, y_var, pred_vars, reff_var = NULL, id_vars,
                               hyper_priors = list("prec.unstruct" = c(1, 5e-4),
                                                   "prec.spatial" = c(1, 5e-4),
                                                   "prec.timerw1" = c(1,0.01)),
                         W_orgUnit,
                        seed = 8675309,
                         nsims = 1){

  #for debugging.remove
  # y_var = "n_case"
  # pred_vars = c("rain_mm", "temp_c")
  # id_vars = c("date", "orgUnit")
  # hyper_priors = list("prec.unstruct" = c(1, 5e-4),
  #                     "prec.spatial" = c(1, 5e-4),
  #                     "prec.timerw1" = c(1,0.01))
  # reff_var = NULL
  # W_orgUnit = prep_caseData(raw_data = demo_malaria,
  #                           y_var = "n_case",
  #                           lagged_vars =  c("rain_mm", "temp_c"),
  #                           scaled_vars = NULL,
  #                           graph_poly = demo_polygon)$W_graph
  #stop debugging code

  prior_setup <- create_inla_setup(hyper_priors)

  if(is.null(reff_var)){
    reff_var <- prior_setup$reff_var
  }

  # ------ set up data ------
  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars, "org_ID",
                                                                  "month_season", "month_num"),
                             remove_NA = TRUE)
  this_analysis <- cv_clean$analysis
  this_analysis$observed <- this_analysis$y_obs
  this_analysis$nsim <- 0
  this_analysis$perm_var = "original"

  #---- create variable importance data ---------
  permute_vars <- pred_vars

  set.seed(seed)
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
                         pred_vars, "nsim", "perm_var"))) |>
    dplyr::mutate(dataset = "varImp")

  both_datasets <- dplyr::bind_rows(this_analysis, permute_data)

  #---- run inla -------------------
  formula_inla <- stats::reformulate(termlabels = c(reff_var, pred_vars),
                                     response = "y_obs")

  inla_mod <- run_inla_config(formula = formula_inla, data = both_datasets,
                              verbose = FALSE, family = 'zeroinflatednbinomial1')

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

create_inla_setup <- function(hyper_priors){
  prior_sp <- list(
    #iid, default = c(1, 5e-4)
    prec.unstruct = list(
      prior = "loggamma",
      param = hyper_priors$prec.unstruct
    ),
    # besag spatial structure parameter, default = c(1, 5e-4)
    prec.spatial = list(
      prior = "loggamma",
      param = hyper_priors$prec.spatial
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

  reff_var <- c("f(month_season, model = 'rw1', cyclic = T, group = org_ID,
      control.group = list(model = 'iid'), scale.model = TRUE,
      hyper = prior_setup$prior_time)",
                "f(org_ID, model = 'bym', graph = W_orgUnit, scale.model = TRUE,
                                 hyper = prior_setup$prior_sp)")

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
#' An internal function to run a configured INLA model
#'
#' @param formula forumula in y ~x format for model
#' @param data data to be used in model
#' @param family family of model to fit. Default = "zeroinflatednbinomial1"
#' @param config whetehr or not to use config for control.compute step. Default = TRUE
#' @param verbose whether fitting should be verbose. Default = TRUE
run_inla_config <- function(formula, data,  family = "zeroinflatednbinomial1",
                            config = T, verbose = T){
  model <- INLA::inla(formula = formula, data = data, family = family,
                      control.inla = list(strategy = 'adaptive', force.diagonal = T),
                      control.compute = list(dic = T, config = config,
                                             waic = T, return.marginals.predictor = T),
                      control.predictor = list(link = 1, compute = T),
                      verbose = verbose)

  # model <- inla.rerun(model) #rerun from hyperparameters above, for publication
  return(model)
}
