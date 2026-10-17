#solving inla issues (or trying)

library(devtools)
load_all()
sf::sf_use_s2(FALSE)

input_path <- testthat::test_path("fixtures", "demo_inputs.Rds")
load(input_path)

#process inputs
inputs <- validate_inputs(config = config,
                              external_data = external_data,
                              disease_data = disease_data,
                              climate_data = climate_data,
                              orgUnit_poly = orgUnit_poly,
                              return_inputs = TRUE)

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
                                              W_graph = data_prep_list$W_graph,
                                              pred_vars = variables$pred_vars,
                                              inla_hyper = inputs$config$inla_hyper,
                                              ranger_hyper = inputs$config$ranger_hyper)
cv_set = forecast_cv
y_var = inputs$config$disease_dataElement
id_vars = c("orgUnit", "date")
quantile_levels = inputs$config$quantile_levels
inla_configs = model_configs$inla


true_data <-  do.call(rbind, get_cv_subsets(cv_set, y_var = y_var,
                                            pred_vars = c(id_vars),
                                            remove_NA = FALSE))
colnames(true_data)[colnames(true_data)== "y_obs"] <- "observed"

fit_inla(cv_set = cv_set,
         y_var = y_var,
         pred_vars = inla_configs$pred_vars,
         id_vars = id_vars,
         reff_var = inla_configs$reff_var,
         hyper_priors = inla_configs$hyper_priors,
         quantile_levels = quantile_levels,
         sample_pi = FALSE,
         W_orgUnit = inla_configs$W_orgUnit,
         n_cores = inla_configs$n_cores,
         verbose = TRUE)
