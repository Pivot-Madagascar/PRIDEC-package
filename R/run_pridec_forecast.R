#' Function to run a PRIDE-C forecast with validated inputs
#' Detail the steps here
#'
#' @param inputs list of config, input_data and orgUnit poly output from validate_inputs
#' @param output_dir directory to save outputs in. If NULL, saves in a temporary directory
#' @returns 0 if success, 1 if not
#' @export

run_pridec_forecast <- function(inputs,
                                output_dir){

  #to avoid edge errors
  suppressMessages(sf::sf_use_s2(FALSE))

  if(is.null(output_dir)){
    output_dir <- tempdir()
  }
  cli::cli_alert_info(paste("Saving intermediate files and output in ", output_dir,
                            "\nDirectory will be created if it does not exist."))

  #if output directory doesn't exist, create it
  if(!(dir.exists(output_dir))){
    dir.create(output_dir)
  }

  #-------- data processing -------
  cli::cli_h1(paste(round(Sys.time()), ": Processing and formatting input data"))
  variables <- format_pred_vars(var_list = inputs$config$pred_vars,
                                input_data = inputs$input_data)

  forecast_start <- as.Date(paste0(inputs$config$forecast_start,"01"), format = "%Y%m%d")

  data_prep_list <- PRIDEC::prep_data(raw_data = inputs$input_data,
                                      y_var = inputs$config$disease_dataElement,
                                      lagged_vars = variables$vars_to_lag,
                                      scaled_vars = variables$vars_to_scale,
                                      lag_n = inputs$config$month_lag,
                                      graph_poly = inputs$graph_poly)

  forecast_cv <- PRIDEC::split_cv_forecast(data_to_split = data_prep_list$data_prep,
                                           forecast_start_date = forecast_start,
                                           month_analysis = inputs$config$month_analysis,
                                           month_assess = inputs$config$month_assess)

  model_configs <- PRIDEC::create_model_configs(model_weights_df = inputs$config$model_weights,
                                        W_graph = inputs$W_graph,
                                        pred_vars = inputs$config$pred_vars,
                                        inla_hyper = inputs$config$inla_hyper,
                                        ranger_hyper = inputs$config$ranger_hyper)


  #------- forecast model
  cli::cli_h1(paste(round(Sys.time()),": Beginning forecast model"))
  cli::cli_alert_info(paste("Forecast period:", forecast_start, "thru", forecast_start + lubridate::period(month = inputs$config$month_assess)))

  stack_forecast <- PRIDEC::ensemble_forecast(cv_set = forecast_cv,
                                              y_var = inputs$config$disease_dataElement,
                                              id_vars = c("orgUnit", "date"),
                                              quantile_levels = inputs$config$quantile_levels,
                                              inla_configs = model_configs$inla,
                                              glm_nb_configs = model_configs$glm_nb,
                                              ranger_configs = model_configs$ranger,
                                              arimax_configs = model_configs$arimax,
                                              naive_configs = model_configs$naive,
                                              return_individual_models = FALSE)

  #reformat for dhis2 instances and quarto report
  dhis2_forecast <- format_forecast_dhis(forecast_out = stack_forecast,
                                         disease_dataElement = inputs$config$disease_dataElement,
                                         forecast_start = inputs$config$forecast_start,
                                         month_assess = inputs$config$month_asses,
                                         month_analysis = inputs$config$month_analysis)

  cli::cli_h1(paste(round(Sys.time()),": Forecast model finished."))

  #--------save intermediate files----------

  cli::cli_alert_info(paste0("Saving intermediate files to ", output_dir))

  #save intermediate files (saving as json for compatability)
  write(jsonlite::toJSON(list("dataValues" = dhis2_forecast)), file.path(output_dir,"forecast.json"))
  saveRDS(forecast_cv, file.path(output_dir, "input_data.RData"))
  write(jsonlite::toJSON(forecast_cv), file.path(output_dir, "input_data.json")) #saves as JSON too for interoperability
  sf::st_write(inputs$graph_poly, file.path(output_dir, "polygon.geojson"),
               delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
  write(jsonlite::toJSON(inputs$config), file.path(output_dir, "config.json"))

  #reset to default
  suppressMessages(sf::sf_use_s2(TRUE))

  return(0)
}


