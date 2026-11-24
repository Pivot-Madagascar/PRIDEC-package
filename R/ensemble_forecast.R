#' Fit an ensemble forecast
#' @param cv_set list of analysis and assessment data to forecast over
#' @param y_var name of y variable
#' @param id_vars name of columns used to identify each sample. Usually time period and orgUnit
#' @param quantile_levels quantile level of prediction intervals. Default = c(0.025, 0.5,0.975)
#'    corresponding to 95% CI
#' @param inla_configs named list of configuration for INLA model. Must include: reff_var, pred_vars,
#'    hyper_priors, W_orgUnit, sample_pi. See \link[PRIDEC]{fit_inla} for more info.
#'    If NULL (default), will not fit an inla model.
#' @param glm_nb_configs named list of configuration for GLM model. Must include: pred_vars
#' @param ranger_configs named list of configuation for ranger model. Must include: pred_vars,
#'    hyper_control. See \link[PRIDEC]{fit_ranger} for more info.
#' @param arimax_configs named list of configuration for ARIMAX model. Must include: pred_vars,
#'    log_trans. See \link[PRIDEC]{fit_arima} for more info.
#' @param naive_configs named list of configuration for naive model. Must include: group_vars.
#'    See \link[PRIDEC]{fit_naive} for more info.
#' @param return_individual_models whether to return intermediate predictions from each model type. Default = FALSE
#' @param abort_on_error if TRUE, an error in any model stops the whole forecast. If FALSE (default),
#'    a model that errors or fails to fit is dropped from the ensemble with a warning and the
#'    remaining models are re-weighted (weights are normalised within each prediction).
#' @param max_pred_multiplier a model is dropped from the ensemble if its predictions are
#'    higher than this multiple of the maximum value observed in the analysis data
#'    (`cv_set$analysis[[y_var]]`) or if any prediction is infinite. The median prediction
#'    is checked if it exists, otherwise all predictions are. This check never aborts the
#'    forecast, regardless of `abort_on_error`. Default = 10.
#'
#' @returns Dataframe of prediction intervals for the ensemble predictions
#' @export
ensemble_forecast <- function(cv_set, y_var, id_vars,
                              quantile_levels = c(0.025,0.5,0.975),
                              inla_configs = NULL,
                              glm_nb_configs = NULL,
                              ranger_configs = NULL,
                              arimax_configs = NULL,
                              naive_configs = NULL,
                              return_individual_models = FALSE,
                              abort_on_error = FALSE,
                              max_pred_multiplier = 10){

  cli::cli_h2("PRIDE-C Forecast Workflow")
  start_time <- Sys.time()
  cli::cli_text(paste0("Started: ", round(Sys.time())))
  true_data <-  do.call(rbind, get_cv_subsets(cv_set, y_var = y_var,
                               pred_vars = c(id_vars),
                               remove_NA = FALSE))
  colnames(true_data)[colnames(true_data)== "y_obs"] <- "observed"

  #ceiling for plausible predictions, based on analysis (training) data only
  max_obs <- suppressWarnings(max(cv_set$analysis[[y_var]], na.rm = TRUE))
  max_allowed <- if(is.finite(max_obs) && max_obs > 0) max_pred_multiplier * max_obs else Inf

  out_list <- list()
  if(!is.null(inla_configs)){
    out_list$inla <- fit_ensemble_member(
      model_name = "inla", label = "INLA", weight = inla_configs$weight,
      abort_on_error = abort_on_error, max_allowed = max_allowed,
      fit_fun =function() fit_inla(cv_set = cv_set,
                                    y_var = y_var,
                                    pred_vars = inla_configs$pred_vars,
                                    id_vars = id_vars,
                                    reff_var = inla_configs$reff_var,
                                    hyper_priors = inla_configs$hyper_priors,
                                    quantile_levels = quantile_levels,
                                    sample_pi = inla_configs$sample_pi,
                                    W_orgUnit = inla_configs$W_orgUnit,
                                    n_cores = inla_configs$n_cores))
  }

  if(!is.null(glm_nb_configs)){
    out_list$glm_nb <- fit_ensemble_member(
      model_name = "glm_nb", label = "GLM", weight = glm_nb_configs$weight,
      abort_on_error = abort_on_error, max_allowed = max_allowed,
      fit_fun =function() fit_glm_nb(cv_set = cv_set,
                                      y_var = y_var,
                                      pred_vars = glm_nb_configs$pred_vars,
                                      id_vars = id_vars,
                                      quantile_levels = quantile_levels))
  }

  if(!is.null(ranger_configs)){
    out_list$ranger <- fit_ensemble_member(
      model_name = "ranger", label = "RANGER", weight = ranger_configs$weight,
      abort_on_error = abort_on_error, max_allowed = max_allowed,
      fit_fun =function() fit_ranger(cv_set = cv_set,
                                      y_var = y_var,
                                      pred_vars = ranger_configs$pred_vars,
                                      id_vars = id_vars,
                                      hyper_control = ranger_configs$hyper_control,
                                      quantile_levels = quantile_levels))
  }

  if(!is.null(arimax_configs)){
    out_list$arimax <- fit_ensemble_member(
      model_name = "arimax", label = "ARIMAX", weight = arimax_configs$weight,
      abort_on_error = abort_on_error, max_allowed = max_allowed,
      fit_fun =function() fit_arima(cv_set = cv_set,
                                     y_var = y_var,
                                     pred_vars = arimax_configs$pred_vars,
                                     log_trans = arimax_configs$log_trans,
                                     quantile_levels = quantile_levels))
  }

  if(!is.null(naive_configs)){
    out_list$naive <- fit_ensemble_member(
      model_name = "naive", label = "NAIVE", weight = naive_configs$weight,
      abort_on_error = abort_on_error, max_allowed = max_allowed,
      fit_fun =function() fit_naive(cv_set = cv_set,
                                     y_var = y_var,
                                     quantile_levels = quantile_levels,
                                     group_vars = naive_configs$group_vars))
  }

  #drop models that failed (NULL), warning has already been issued for each
  out_list <- purrr::compact(out_list)
  if(length(out_list) == 0){
    stop("All models in the ensemble failed to fit. Cannot create a forecast.", call. = FALSE)
  }
  cli::cli_alert_info("Ensemble contains {length(out_list)} model{?s}: {paste(names(out_list), collapse = ', ')}")


  cli::cli_h3("Creating stacked ensemble")
  #combine to get ensemble based on weights
  model_stack <- split(dplyr::bind_rows(out_list), f = ~ orgUnit + date + quant_long) |>
    purrr::map(\(x) sample_preds(predicted = x$predicted,
                          weight = x$weight)) |>
    dplyr::bind_rows(,.id = "list_id") |>
    tidyr::separate(.data$list_id, into = c("orgUnit", "date", "quant_long"),
                    sep = "\\.",
                    extra = "merge", fill = "left") |>
    dplyr::mutate(quantile_level = as.numeric(gsub("quant_","", .data$quant_long))) |>
    dplyr::mutate(date = as.Date(date))

  stack_out <- dplyr::left_join(model_stack, true_data, by = id_vars)

  cli::cli_alert_info(paste0("Finished ensemble stack. Time elapsed: ",
                             round(as.numeric(Sys.time() - start_time, units = "secs")), " seconds."))
  if(return_individual_models){
    return(list(mod_outputs = out_list,
                stack = stack_out))
  } else {
    return(stack_out)
  }
}

#'Utility function to get weighted means of predictions
#' @param predicted vector of predicted values
#' @param weight vector of weights
#' @returns mean prediction in a list
sample_preds <- function(predicted, weight){
  list("predicted" = stats::weighted.mean(predicted, weight, na.rm = TRUE))
}

#' Fit one member of the ensemble, dropping it (with a warning) if it fails
#'
#' @param model_name name used for the `model` column and list element
#' @param label name used in messages
#' @param weight ensemble weight of this model
#' @param fit_fun function of no arguments that fits the model and returns a prediction dataframe
#'    (or NULL if the model could not be fit)
#' @param abort_on_error whether to re-throw errors instead of dropping the model
#' @param max_allowed largest prediction allowed before the model is dropped. Default = Inf
#' @returns dataframe of predictions with `model` and `weight` columns, or NULL if the model was dropped
#' @noRd
fit_ensemble_member <- function(model_name, label, weight, fit_fun, abort_on_error = FALSE,
                                max_allowed = Inf){
  cli::cli_h3("{label} Forecast")
  cli::cli_text(paste0("Started: ", round(Sys.time())))

  drop_model <- function(reason){
    warning(label, " model removed from the ensemble: ", reason,
            "\nForecast will be made with the remaining models.", call. = FALSE)
    NULL
  }

  preds <- tryCatch(
    fit_fun(),
    error = function(e){
      if(abort_on_error){
        stop(paste(label, "model failed with error message:\n", conditionMessage(e)),
             call. = FALSE)
      }
      structure(list(reason = paste0("it failed with error message: ", conditionMessage(e))),
                class = "ensemble_member_failed")
    })

  if(inherits(preds, "ensemble_member_failed")) return(drop_model(preds$reason))
  if(is.null(preds)) return(drop_model("it did not fit correctly (e.g. convergence issues)."))
  if(!is.data.frame(preds) || nrow(preds) == 0 || all(is.na(preds$predicted))){
    return(drop_model("it returned no valid predictions."))
  }
  if(any(is.infinite(preds$predicted))){
    return(drop_model("it returned infinite predictions."))
  }
  #check the median if it exists, otherwise all rows
  is_median <- preds$quant_long %in% "quant_0.5"
  check_preds <- if(any(is_median)) preds$predicted[is_median] else preds$predicted
  if(any(check_preds > max_allowed, na.rm = TRUE)){
    return(drop_model(paste0("its predictions were higher than the maximum allowed value of ",
                             round(max_allowed, 2), " (a multiple of the maximum observed value).")))
  }

  preds$model <- model_name
  preds$weight <- weight
  preds
}
