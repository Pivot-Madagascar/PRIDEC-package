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
                         n_cores = ncores_default()){
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
                                       n_cores = ncores_default()){

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
                               n_cores = ncores_default()){

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
