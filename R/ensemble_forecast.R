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
ensemble_forecast <- function(cv_set, y_var, id_vars,
                              quantile_levels = c(0.025,0.5,0.975),
                              inla_configs = NULL,
                              glm_nb_configs = NULL,
                              ranger_configs = NULL,
                              arimax_configs = NULL,
                              naive_configs = NULL){

  true_data <-  do.call(rbind, get_cv_subsets(cv_set, y_var = y_var,
                               pred_vars = c(id_vars),
                               remove_NA = FALSE))
  colnames(true_data)[colnames(true_data)== "y_obs"] <- "observed"

  out_list <- list()
  if(!is.null(inla_configs)){
    pred_inla <- fit_inla(cv_set = cv_set,
                          y_var = y_var,
                          pred_vars = inla_configs$pred_vars,
                          id_vars = id_vars,
                          reff_var = inla_configs$reff_var,
                          hyper_priors = inla_configs$hyper_priors,
                          quantile_levels = quantile_levels,
                          sample_pi = inla_configs$sample_pi,
                          W_orgUnit = inla_configs$W_orgUnit)
    pred_inla$model <- "inla"
    pred_inla$weight <- inla_configs$weight
    out_list$inla <- pred_inla
  } else  out_list$inla <- NULL

  if(!is.null(glm_nb_configs)){
    pred_glm_nb <- fit_glm_nb(cv_set = cv_set,
                          y_var = y_var,
                          pred_vars = glm_nb_configs$pred_vars,
                          id_vars = id_vars,
                          quantile_levels = quantile_levels)
    pred_glm_nb$model <- "glm_nb"
    pred_glm_nb$weight <- glm_nb_configs$weight
    out_list$glm_nb <- pred_glm_nb
  } else out_list$glm_nb <- NULL

  if(!is.null(ranger_configs)){
    pred_ranger <- fit_ranger(cv_set = cv_set,
                              y_var = y_var,
                              pred_vars = ranger_configs$pred_vars,
                              id_vars = id_vars,
                              hyper_control = ranger_configs$hyper_control,
                              quantile_levels = quantile_levels)
    pred_ranger$model <- "ranger"
    pred_ranger$weight <- ranger_configs$weight
    out_list$ranger <- pred_ranger
  } else out_list$ranger <- NULL

  if(!is.null(arimax_configs)){
    pred_arimax <- fit_arima(cv_set = cv_set,
                              y_var = y_var,
                              pred_vars = arimax_configs$pred_vars,
                              log_trans = arimax_configs$log_trans,
                              quantile_levels = quantile_levels)
    pred_arimax$model <- "arimax"
    pred_arimax$weight <- arimax_configs$weight
    out_list$arimax <- pred_arimax
  } else out_list$arimax <- NULL

  if(!is.null(naive_configs)){
    pred_naive <- fit_naive(cv_set = cv_set,
                             y_var = y_var,
                             group_vars = naive_configs$group_vars)
    pred_naive$model <- "naive"
    pred_naive$weight <- naive_configs$weight
    out_list$naive <- pred_naive
  } else out_list$naive<- NULL

  #combine to get ensemble based on weights
  model_stack <- split(dplyr::bind_rows(out_list), f = ~ orgUnit + date + quant_long) |>
    purrr::map(\(x) sample_preds(predicted = x$predicted,
                          weight = x$weight)) |>
    dplyr::bind_rows(,.id = "list_id") |>
    tidyr::separate(.data$list_id, into = c("orgUnit", "date", "quant_long"),
                    sep = "\\.",
                    extra = "merge", fill = "left") |>
    dplyr::mutate(quantile_level = as.numeric(gsub("quant_","", .data$quant_long))) |>
    mutate(date = as.Date(date))

  stack_out <- dplyr::left_join(model_stack, true_data, by = id_vars)

  return(stack_out)
}

#'Utility function to get weighted means of predictions
#'@param predicted vector of predicted values
#'@param weight vector of weights
sample_preds <- function(predicted, weight){
  list("predicted" = stats::weighted.mean(predicted, weight, na.rm = TRUE))
}
