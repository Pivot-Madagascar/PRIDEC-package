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
#' @param create_report Whether to create the HTML report. Default = FALSE
#' @param report_configs Optional configurations for quarto_report provided as a named list. Options are:
#'   html_filename, doc_title, lang
#' @returns saves model outputs to `results_dir`. Creates a quarto doc of model outputs saved in `results_dir`
#' @export
train_models <- function(prep_output,
                         models,
                         y_var,
                         pred_vars,
                         id_vars = c("orgUnit", "date"),
                         results_dir = NULL,
                         tune = NULL,
                         model_configs = NULL,
                         create_report = FALSE,
                         report_configs = NULL){

  cli::cli_h1("PRIDE-C Train Models Workflow")

  # for debugging and testing
  # tune = NULL
  # y_var = "n_case"
  # pred_vars = c("pev_lagsc", "rain_mm_lagsc", "temp_c_lagsc",
  #               "wealth_indexsc", "elevationsc",
  #               "time_to_districtsc")
  # id_vars = c("orgUnit", "date")
  # results_dir = NULL
  # models = c("naive", "arimax", "ranger")
  # results_dir = paste(tempdir(), "pridec-output", sep = "/")

  #create logical for if fitting all 5 models
  fitting_five_models <- sum((c("naive", "ranger", "inla", "glm_nb", "arimax") %in% models))==5

  #move this outside the function
  if(is.null(results_dir)){
    results_dir <- paste(tempdir(), "pridec-output", sep = "/")
  }

  if(!dir.exists(results_dir)){
   cli::cli_alert_warning(paste0("{.file ", results_dir, "}", " does not exist. Creating now."))
    dir.create(results_dir)
  }

  if(is.null(report_configs)){
    report_configs <- list("html_filename" = NULL,
                           "lang" = "fr",
                           "doc_title" = NULL)
  }
  if(is.null(report_configs$html_filename)){
    report_configs$html_filename <- file.path(results_dir, "quarto-report.html")
  }

  #create log file within directory
  lc_filename <- paste0(results_dir, "/log.log")
  file.create(lc_filename)
  log_con <- file(lc_filename,
                  open = "a")
  cli::cli_alert_info(paste0("Saving log to: {.file ", lc_filename, "}"))
  if(create_report & fitting_five_models){
  cli::cli_alert_info(paste0("Saving HTML Report at: {.file ", report_configs$html_filename, "}"))
  cat(paste0("Saving quarto report at ", report_configs$html_filename),
      file = log_con, sep = "\n")
  } else {
    cli::cli_alert_info(paste0("No HTML report being created."))
    cat("No HTML report created", file = log_con, sep = "\n")
  }

  cli::cli_text("Fitting the following models:\n",
          paste(models, collapse = " | "))

  cv_setList <- split_cv_rolling(prep_output$data_prep,
                                 month_analysis = 60,
                                 month_assess = 3)

  #---- Naive ------
  if("naive" %in% models){
    cat(paste("Beginning naive model fit at", round(Sys.time())),
        file = log_con, sep = "\n")
    cli::cli_h2("NAIVE model fit")
    cli::cli_text(paste0("Beginning naive model fit at ", round(Sys.time())))


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

      cat(paste("Finished naive model fit at", round(Sys.time())),
          file = log_con, sep = "\n")
      cli::cli_alert_success(paste0("Finished naive model fit at ", round(Sys.time())))
  }

  # ------- random forest (ranger) ------------
  if("ranger" %in% models){
    cli::cli_h2("RANGER model fit")
    cli::cli_text(paste0("Beginning ranger model fit at ", round(Sys.time())))
    #add message for configs?
    cat(paste("Beginning ranger model fit at", round(Sys.time())),
        file = log_con, sep = "\n")


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

    cat(paste("Completed ranger model fit at", round(Sys.time())),
        file = log_con, sep = "\n")
    cli::cli_alert_success(paste0("Completed ranger model fit at ", round(Sys.time())))
  }

  #--------------Arimax ------------

  if("arimax" %in% models){

    cli::cli_h2("ARIMAX model fit")
    cli::cli_text(paste0("Beginning arimax model fit at ", round(Sys.time())))
    #add message for configs?
    cat(paste("Beginning arimax model fit at", round(Sys.time())),
        file = log_con, sep = "\n")

    #identify dynamic pred_vars
    arima_vars <- prep_output$data_prep[,c(pred_vars, "orgUnit")]
    arima_vars <- lapply(split(arima_vars, ~orgUnit), FUN = function(x) {
      unlist(lapply(apply(x,2, unique), length))
    }) |>
      dplyr::bind_rows() |>
      colMeans()
    arima_vars <- names(arima_vars)[arima_vars>3]

    #add counter to this because it can take so long
      arima_preds <- purrr::imap(cv_setList,
                                .f= function(x, y){
                                  cat(paste("Fitting ARIMAX CV", y, "at", round(Sys.time())),
                                      file = log_con, sep = "\n")

                                  this_fit <- fit_arima(cv_set = x,
                                            y_var = y_var,
                                            pred_vars = arima_vars,
                                            log_trans = TRUE)
                                  return(this_fit)
                                })

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

      cli::cli_alert_success(paste0("Completed arimax model fit at ", round(Sys.time())))
      cat(paste("Completed arimax model fit at", round(Sys.time())),
          file = log_con, sep = "\n")
  }

  #----------------------glm_nb ---------------------
  if("glm_nb" %in% models){

    cli::cli_h2("GLM model fit")
    cli::cli_text(paste0("Beginning glm model fit at ", round(Sys.time())))
    cat(paste("Beginning glm model fit at", round(Sys.time())),
        file = log_con, sep = "\n")

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

    cli::cli_alert_success(paste0("Completed glm model fit at ", round(Sys.time())))
    cat(paste("Completed glm model fit at", round(Sys.time())),
        file = log_con, sep = "\n")
  }

  # ----------------------inla--------------------
  if("inla" %in% models){

    cli::cli_h2("INLA model fit")
    cli::cli_text(paste0("Beginning inla model fit at ", round(Sys.time())))
    cat(paste("Beginning inla model fit at", round(Sys.time())),
        file = log_con, sep = "\n")

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

    #use first orgUnit alphabetically and median date for inv_variables in last cv_set
    n_cv <- length(cv_setList)
    inv_org <- sort(unique(cv_setList[[n_cv]]$analysis$orgUnit))[1]
    org_df <- subset(cv_setList[[n_cv]]$analysis, orgUnit == inv_org)
    org_df <- org_df[!is.na(org_df[[y_var]]),]
    all_dates <- sort(unique(org_df$date))
    inv_date <- all_dates[floor(length(all_dates)/2)]

    inla_inv_var <- inv_variables_inla(cv_set = cv_setList[[n_cv]],
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

    cli::cli_alert_success(paste0("Completed inla model fit at ", round(Sys.time())))
    cat(paste("Completed inla model fit at", round(Sys.time())),
        file = log_con, sep = "\n")


  }

  #-------save supporting files ----------------#
  #save polygons for mapping
  #needs to be updated to be dynamic
  saveRDS(demo_polygon, paste0(results_dir, "/orgPolygon.Rdata"))
  saveRDS(prep_output, paste0(results_dir, "/prep_output.Rdata"))
  var_info <- list("y_var" = y_var,
                   "pred_vars" = pred_vars)
  saveRDS(var_info, paste0(results_dir, "/var_info.Rdata"))

  cli::cli_h2(paste0("Completed training ", length(models), " models at ", round(Sys.time())))
  cat(paste0("Completed training ", length(models), " models at ", round(Sys.time())),
      file = log_con, sep = "\n")

  # --------------------Create quarto report ------------------------- #
  if(create_report){

  if(fitting_five_models){
  cli::cli_h2("Creating report of model performance")
  cat(paste0("Creating quarto report at ", round(Sys.time())),
      file = log_con, sep = "\n")

  create_pridec_quarto(results_dir = results_dir,
                       html_filename = report_configs$html_filename,
                       lang = report_configs$lang,
                       doc_title = report_configs$doc_title)
  } else {
    cli::cli_alert_warning("Cannot create HTML report unless all 5 models are fit.
                           Please retrain with all 5 models to output report.")
  }

  }

  cli::cli_h2(paste0("Finished model training workflow at ", round(Sys.time())))
  cat(paste0("Finished model training workflow at ", round(Sys.time())),
      file = log_con, sep = "\n")

}

#' Function to create quarto doc from model outputs
#' @param results_dir path to directory where model outputs are saved
#' @param html_filename where you want the html file to be saved, including path
#' @param language which language template to use. currently only  "fr" available
#' @param doc_title title for HTML report. Default: "PRIDEC Rapport de Performance"
create_pridec_quarto <- function(results_dir,
                                 html_filename,
                                 lang = "fr",
                                 doc_title = NULL){

  #for debug
  # results_dir <- "/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/demo_trainModelResults"
  # html_filename <- "/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/quarto-out-test.html"
  # lang = "fr"
  # doc_title = NULL

  if(is.null(doc_title)){
    doc_title <- "PRIDEC Rapport de Performance"
  }

  template_file <- system.file(paste0("quarto_templates/modelPerformance-template_", lang,".qmd"),
                               package = "PRIDEC")

  file.copy(template_file, to = "tmp_template.qmd",
            overwrite = TRUE)

  quarto::quarto_render(
    input = "tmp_template.qmd",
    output_file = "tmp_quarto-out.html",
    execute_params = list(results_dir = results_dir),
    quarto_args = c("--metadata", paste0("title=", doc_title))
  )
  file.copy("tmp_quarto-out.html", to = html_filename,
            overwrite = TRUE)
  file.remove("tmp_template.qmd")
  file.remove("tmp_quarto-out.html")

}


