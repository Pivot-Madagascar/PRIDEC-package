test_that("train model works", {
  #don't run this on CRAN, it will take ages

  data(demo_malaria)
  data(demo_polygon)

  prep_output <- prep_data(raw_data = demo_malaria,
                           y_var = "n_case",
                           lagged_vars = c("pev", "rain_mm", "temp_c"),
                           scaled_vars = c("pev", "rain_mm", "temp_c",
                                           "wealth_index", "elevation",
                                           "time_to_district"),
                           lag_n = 3,
                           graph_poly = demo_polygon)

  cv_setList <- split_cv_rolling(prep_output$data_prep,
                                 month_analysis = 60,
                                 month_assess = 3)[1:8]

})
