test_that("full ranger RF workflow work", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_caseData(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("rain_mm", "temp_c"),
                                                           scaled_vars = NULL,
                                                           graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[23]]

  #----- fit model ------

  expect_no_condition(
  test_rf <- fit_ranger(cv_set, y_var = "n_case",
                        id_vars = c("orgUnit", "date"),
                        pred_vars = c("rain_mm", "temp_c", "month_season", "orgUnit"))
  )
  expect_equal(class(test_rf$predicted), "numeric")
  expect_contains(names(test_rf), c("orgUnit", "date", "observed", "dataset", "quant_long", "predicted",
                                    "quantile_level"))

  #---- evaluate model -----
  expect_type(eval_performance(test_rf)$prop_over, "double")
  expect_equal(eval_performance(test_rf)$dataset, c("analysis", "assess"))

  #-----investigate variables ----
  var_scales <- prep_caseData(raw_data = demo_malaria,
                                y_var = "n_case",
                                lagged_vars =  c("rain_mm", "temp_c"),
                                scaled_vars = NULL,
                                graph_poly = NULL)$scale_factors
  expect_no_condition(
  test_inv <- inv_variables_ranger(cv_set, y_var = "n_case",
                       id_vars = c("orgUnit", "date"),
                       pred_vars = c("rain_mm", "temp_c", "month_season", "orgUnit"),
                       var_scales = var_scales)
  )
  expect_equal(nrow(test_inv$var_imp),4)
  expect_contains(names(test_inv$counter_data[[1]]), c("var_valuesc", "yhat", "variable", "var_value"))

  #----- visualization -------
  #predictions
  expect_no_condition(
    plot_predictions(test_rf)
  )

  #pdp plots
  cf_data <- test_inv$counter_data[[2]]
  expect_no_condition(
  plot_counterfactual_one(cf_data, var_label = "temperature")
  )
  #plotting multiple
  multi_plots <- plot_counterfactual(test_inv$counter_data, y_range = c(0,50))
  expect_contains(class(multi_plots[[1]]), "ggplot")

})

test_that("ranger tuning",{
  data(demo_malaria)
  data(demo_polygon)

  cvfold_list <- split_cv_rolling(data_to_split = prep_caseData(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("rain_mm", "temp_c"),
                                                           scaled_vars = NULL,
                                                           graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[1:5]

  test_tune <- tune_ranger(cvfold_list = cvfold_list,
                           y_var = "n_case",
                           id_vars = c("orgUnit", "date"),
                           pred_vars = c("rain_mm", "temp_c", "month_season", "orgUnit"),
                           metric = "wape",
                           tune_grid = expand.grid(m.try = 3,
                                                   min.node.size = 3:5,
                                                   num.trees = 100))
  expect_contains(names(test_tune), "metric")



})
