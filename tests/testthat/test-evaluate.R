test_that("evaluate function on naive model", {
  #eventually save predictions to mroe easily load?
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_caseData(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("pev", "rain_mm", "temp_c"),
                                                           scaled_vars = c("wealth_index", "elevation",
                                                                           "LLIN_use", "time_to_district", "LLIN_wane"),
                                                           graph_poly = demo_polygon)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[4]]
  #test naive model
  pred_naive <- fit_naive(cv_set = cv_set,
                              y_var = "n_case",
                              group_vars = c("orgUnit", "month_season"))
  expect_no_condition(eval_performance(pred_naive))
  #ensure that the wis function in scoringutils hasn't changed
  expect_equal(round(eval_performance(pred_naive)$wis,3), c(3.734, 42.991))

})


