#' Run full train_model workflow using multiple modeling approaches
#' @param prep_output output of prep_data
#' @param models vector of models to fit. Options: naive, arimax, glm_nb, ranger, inla
#' @param y_var character. name of variable to predict
#' @param pred_vars character vector of predictor variables. Not all will be used
#'   in all models
#' @param results_dir directory to save model results and quarto document in.
#'   If NULL, temporary directory will be used
#' @param tune string vector containing names of models to tune. Options = ranger
#' @param model_configs Optional list of configurations for each model. Name of
#'   element in list should match model name. See \link[PRIDEC]{ensemble_forecast}
#'   for more info.
#' @returns saves model outputs to `results_dir` if specified. Creates a quarto doc of
#'   model outputs
train_models <- function(prep_output,
                         models,
                         y_var,
                         pred_vars,
                         id_vars = c("orgUnit", "date"),
                         results_dir = NULL,
                         tune = NULL,
                         model_configs){

  # for debugging and testing
  tune = NULL
  y_var = "n_case"
  pred_vars = c("pev_lagsc", "rain_mm_lagsc", "temp_c_lagsc",
                "wealth_indexsc", "elevationsc",
                "time_to_districtsc")
  id_vars = c("orgUnit", "date")




  if(is.null(results_dir)){
    results_dir <- paste(tempdir(), "pridec-output", sep = "/")
  }

  if(!dir.exists(results_dir)){
    dir.create(results_dir)
  }


  cv_setList <- split_cv_rolling(prep_output$data_prep,
                                 month_analysis = 60,
                                 month_assess = 3)

  #---- Naive ------
  if("naive" %in% models){
    #predictions
      naive_preds <- purrr::map(1:length(cv_setList),
                                \(x) fit_naive(cv_set = cv_setList[[x]],
                                               y_var = y_var,
                                               group_vars = c("orgUnit", "month_season")) |>
                                  dplyr::mutate(cv_fold = x))
      # naive_preds <- do.call(rbind, naive_preds)
    #evaluation, probably want to get the mean of this across folds
      naive_perf <-  purrr::map(1:length(naive_preds),
                                \(x) eval_performance(naive_preds[[x]]) |>
                                  dplyr::mutate(cv_fold = x)) |>
        dplyr::bind_rows()

      saveRDS(naive_preds, paste(results_dir, "/naive_preds.Rdata"))
      saveRDS(naive_perf, paste(results_dir, "/naive_perf.Rdata"))
  }

  # ------- random forest (ranger) ------------
  if("ranger" %in% models){
    rf_pred_vars <- unique(c(pred_vars, "month_num", "month_season", "orgUnit"))

    ranger_preds <- purrr::map(1:length(cv_setList),
                               \(x) fit_ranger(cv_set = cv_setList[[x]],
                                               y_var = y_var,
                                               pred_vars = rf_pred_vars,
                                               id_vars = id_vars) |>
                                 dplyr::mutate(cv_fold = x))
    ranger_perf <-  purrr::map(1:length(ranger_preds),
                              \(x) eval_performance(ranger_preds[[x]]) |>
                                dplyr::mutate(cv_fold = x)) |>
      dplyr::bind_rows()

    ranger_inv_var <- inv_variables_ranger(cv_set = cv_setList[[length(cv_setList)]],
                                           y_var = y_var,
                                           id_vars = id_vars,
                                           pred_vars = rf_pred_vars,
                                           var_scales = prep_output$scale_factors)

    saveRDS(ranger_preds, paste(results_dir, "/ranger_preds.Rdata"))
    saveRDS(ranger_perf, paste(results_dir, "/ranger_perf.Rdata"))
    saveRDS(ranger_inv_var, paste(results_dir, "/ranger_inv_var.Rdata"))
  }

  #--------------Arima ------------

  if("arimax" %in% models){

    #identify dynamic pred_vars
    arima_vars <- prep_output$data_prep[,c(pred_vars, "orgUnit")]
    arima_vars <- lapply(split(arima_vars, ~orgUnit), FUN = function(x) {
      unlist(lapply(apply(x,2, unique), length))
    }) |>
      dplyr::bind_rows() |>
      colMeans()
    arima_vars <- names(arima_vars)[arima_vars>3]


      arima_preds <- purrr::map(cv_setList,
                                .f= ~fit_arima(cv_set = .x,
                                                y_var = y_var,
                                                pred_vars = arima_vars,
                                               log_trans = TRUE))

      arima_perf <- purrr::map(1:length(arima_preds),
                 \(x) eval_performance(arima_preds[[x]]) |>
                   dplyr::mutate(cv_fold = x)) |>
        dplyr::bind_rows()


      arima_inv_var <- inv_variables_arima(cv_set = cv_setList[[length(cv_setList)]],
                                             y_var = y_var,
                                             pred_vars = arima_vars,
                                             var_scales = prep_output$scale_factors)

      saveRDS(arima_preds, paste(results_dir, "/arima_preds.Rdata"))
      saveRDS(arima_perf, paste(results_dir, "/arima_perf.Rdata"))
      saveRDS(arima_inv_var, paste(results_dir, "/arima_inv_var.Rdata"))

  }



}
