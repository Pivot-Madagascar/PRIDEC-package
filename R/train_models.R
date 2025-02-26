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
                         results_dir,
                         tune = NULL,
                         model_configs){

  # for debugging and testing
  # tune = NULL
  # y_var = "n_case"
  # pred_vars = c("pev_lagsc", "rain_mm_lagsc", "temp_c_lagsc",
  #               "wealth_indexsc", "elevationsc",
  #               "time_to_districtsc")
  # id_vars = c("orgUnit", "date")
  # results_dir = NULL



  #move this outside the function
  # if(is.null(results_dir)){
  #   results_dir <- paste(tempdir(), "pridec-output", sep = "/")
  # }
  #
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

      saveRDS(naive_preds, paste0(results_dir, "/naive_preds.Rdata"))
      saveRDS(naive_perf, paste0(results_dir, "/naive_perf.Rdata"))
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

    saveRDS(ranger_preds, paste0(results_dir, "/ranger_preds.Rdata"))
    saveRDS(ranger_perf, paste0(results_dir, "/ranger_perf.Rdata"))
    saveRDS(ranger_inv_var, paste0(results_dir, "/ranger_inv_var.Rdata"))
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

      saveRDS(arima_preds, paste0(results_dir, "/arima_preds.Rdata"))
      saveRDS(arima_perf, paste0(results_dir, "/arima_perf.Rdata"))
      saveRDS(arima_inv_var, paste0(results_dir, "/arima_inv_var.Rdata"))

  }

  #----------------------glm_nb ---------------------
  if("glm_nb" %in% models){

    glm_preds <- purrr::map(cv_setList,
               .f= ~fit_glm_nb(cv_set = .x,
                              y_var = y_var,
                              pred_vars = pred_vars,
                              id_vars = c("date", "orgUnit")))

    glm_perf <- purrr::map(1:length(glm_preds),
                           \(x) eval_performance(glm_preds[[x]]) |>
                             dplyr::mutate(cv_fold = x)) |>
      dplyr::bind_rows()

    glm_inv_var <- inv_variables_glm_nb(cv_set = cv_setList[[length(cv_setList)]],
                                        y_var = y_var,
                                        pred_vars = pred_vars,
                                        id_vars = c("date", "orgUnit"),
                                        nsim = 50,
                                        var_scales = prep_output$scale_factors)

    saveRDS(glm_preds, paste0(results_dir, "/glm_preds.Rdata"))
    saveRDS(glm_perf, paste0(results_dir, "/glm_perf.Rdata"))
    saveRDS(glm_inv_var, paste0(results_dir, "/glm_inv_var.Rdata"))
  }

  # ----------------------inla--------------------
  if("inla" %in% models){

    inla_preds <- purrr::map(cv_setList,
                            .f= ~fit_inla(cv_set = .x,
                                            y_var = y_var,
                                            pred_vars = pred_vars,
                                            id_vars = c("date", "orgUnit"),
                                          W_orgUnit = prep_output$W_graph))

    inla_perf <- purrr::map(1:length(inla_preds),
                            \(x) eval_performance(inla_preds[[x]]) |>
                              dplyr::mutate(cv_fold = x)) |>
      dplyr::bind_rows()

    #use first orgUnit alphabetically and median date for inv_variables
    inv_org <- unique(cv_setList[[1]]$analysis$orgUnit)[1]
    all_dates <- unique(cv_setList[[1]]$analysis$date)
    inv_date <- all_dates[floor(length(all_dates)/2)]

    inla_inv_var <- inv_variables_inla(cv_set = cv_setList[[1]],
                                       y_var = y_var,
                                       pred_vars = pred_vars,
                                       id_vars = c("date", "orgUnit"),
                                       W_orgUnit = prep_output$W_graph,
                                       constant_org = inv_org,
                                       constant_date = inv_date,
                                       seed = 8675309,
                                       nsims = 5,
                                       var_scales = prep_output$scale_factors)

    saveRDS(inla_preds, paste0(results_dir, "/inla_preds.Rdata"))
    saveRDS(inla_perf, paste0(results_dir, "/inla_perf.Rdata"))
    saveRDS(inla_inv_var, paste0(results_dir, "/inla_inv_var.Rdata"))

  }

  #-------save supporting files ----------------#
  #save polygons for mapping
  #needs to be updated to be dynamic
  saveRDS(demo_polygon, paste0(results_dir, "/orgPolygon.Rdata"))
  saveRDS(prep_output, paste0(results_dir, "/prep_output.Rdata"))
  var_info <- list("y_var" = y_var,
                   "pred_vars" = pred_vars)
  saveRDS(var_info, paste0(results_dir, "/var_info.Rdata"))

}

#' Function to create quarto doc from model outputs
#' @param results_dir path to directory where model outputs are saved. cannot be relative
#' @param html_filename where you want the html file to be saved. cannot be relative
create_pridec_quarto <- function(results_dir,
                                 html_filename){

  #for debug
  results_dir <- "/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/demo_trainModelResults"
  html_filename <- "/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/quarto-out-test.html"


}


