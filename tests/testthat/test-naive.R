test_that("naive fit works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                       y_var = "n_case",
                                       lagged_vars =  c("pev", "rain_mm", "temp_c"),
                                       scaled_vars = c("wealth_index", "elevation",
                                                       "LLIN_use", "time_to_district", "LLIN_wane"),
                                       graph_poly = demo_polygon)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[4]]

  pred_intervals <- fit_naive(cv_set = cv_set,
            y_var = "n_case",
            group_vars = c("orgUnit", "month_season"))

  expect_equal(colnames(pred_intervals), c("orgUnit", "date", "dataset", "observed", "predicted", "quant_long",
                                           "quantile_level"))
  expect_equal(sum(is.na(pred_intervals$predicted)), 0)

  #evaluate performance
  eval_performance(pred_intervals)
})
