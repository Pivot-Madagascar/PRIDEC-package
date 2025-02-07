test_that("ensemble forecast works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                       y_var = "n_case",
                                                       lagged_vars =  c("rain_mm", "temp_c"),
                                                       scaled_vars = NULL,
                                                       graph_poly = demo_polygon)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[14]]
  W_orgUnit <- prep_data(raw_data = demo_malaria,
                         y_var = "n_case",
                         lagged_vars =  c("rain_mm", "temp_c"),
                         scaled_vars = NULL,
                         graph_poly = demo_polygon)$W_graph
  #create configs for each model
  inla_configs <- list(reff_var = NULL, pred_vars = c("rain_mm", "temp_c"),
                       hyper_priors = list("prec.unstruct" = c(1, 5e-4),
                                           "prec.spatial" = c(1, 5e-4),
                                           "prec.timerw1" = c(1,0.01)),
                       W_orgUnit = W_orgUnit, sample_pi = FALSE,
                       weight = 0.5)
  glm_nb_configs <- list(pred_vars = c("rain_mm", "temp_c"),
                         weight = 0.1)
  ranger_configs <- list(pred_vars = c("rain_mm", "temp_c"),
                         hyper_control = list("mtry" = NULL, "min.node.size" = NULL, "num.trees" = 500),
                         weight = 0.05)
  arimax_configs <- list(pred_vars = c("rain_mm", "temp_c"),
                         log_trans = TRUE,
                         weight = 0.2)
  naive_configs <- list(group_vars = c("month_season", "orgUnit"),
                        weight = 0.02)

  #run the forecast
  stack_forecast <- ensemble_forecast(cv_set = cv_set,
                                      y_var = "n_case",
                                      id_vars = c("orgUnit", "date"),
                                      inla_configs = inla_configs,
                                      glm_nb_configs = glm_nb_configs,
                                      ranger_configs = ranger_configs,
                                      arimax_configs = arimax_configs,
                                      naive_configs = naive_configs)
  expect_contains(colnames(stack_forecast), c("orgUnit", "predicted", "observed", "quantile_level"))

  plot_predictions(stack_forecast, quantile_ribbon = c(0.025, 0.975))

})
