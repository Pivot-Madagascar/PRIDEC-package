test_that("get_cv_subsets", {
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
  expect_no_condition({
    test_out <- get_cv_subsets(cv_set, y_var = "n_case", pred_vars = c("rain_mm", "LLIN_use"))
  })

  expect_equal(names(test_out), c("analysis", "assess"))
  expect_contains(names(test_out$assess), c("rain_mm", "LLIN_use", "y_obs"))

  #check it removes the NAs appropriately
  test_noNA <- get_cv_subsets(cv_set, y_var = "n_case", pred_vars = c("rain_mm", "LLIN_use"),
                              remove_NA = TRUE)
  expect_equal(sum(is.na(dplyr::bind_rows(test_noNA)$y_obs)),0)
})



