test_that("data preparation function works", {
  data(demo_malaria)
  data(demo_polygon)
  lagged_vars = c("pev", "rain_mm", "temp_c")
  scaled_vars = c("wealth_index", "elevation",
                  "LLIN_use", "time_to_district", "LLIN_wane")
  lag_n = 3

  test1 <- prep_caseData(raw_data = demo_malaria, y_var = "n_case",
                         lagged_vars = lagged_vars, scaled_vars = scaled_vars,
                         graph_poly = demo_polygon)

  # NOT FINISHED YET, WIP
})

test_that("seasonal imputation works", {

})
