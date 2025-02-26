test_that("full glm_nb workflow works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("rain_mm", "temp_c"),
                                                           scaled_vars = NULL,
                                                           graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[16]]

  expect_no_condition(
    test_fit <- fit_glm_nb(cv_set,
                          y_var = "n_case",
                          id_vars = c("date", "orgUnit"),
                          pred_vars = c("rain_mm", "temp_c"))
  )
  #check performance
  expect_equal(eval_performance(test_fit)$wape, c(0.532286544683763, 0.72950493946477))
  #check plot
  expect_contains(class(plot_predictions(test_fit[test_fit$orgUnit %in% c("CSB2 ATSINDRA", "CSB2 IFANADIANA"),])),
                  "ggplot")
})

test_that("glm.nb variable exploration works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("rain_mm", "temp_c"),
                                                           scaled_vars = NULL,
                                                           graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[3]]


  test_inv <- inv_variables_glm_nb(cv_set = cv_set,
                                   y_var = "n_case",
                                   id_vars = c("orgUnit", "date"),
                                   pred_vars = c("rain_mm", "temp_c"),
                                   nsim = 50)

  expect_type(test_inv$varImp$importance, "double")
  expect_equal(length(test_inv$counter_data), 4)
  expect_no_condition(plot_counterfactual_one(test_inv$counter_data[[3]], var_label = "rain"))
  expect_equal(sum(test_inv$varImp$importance),1)
  expect_true(all(test_inv$varImp$importance>=0))

})
