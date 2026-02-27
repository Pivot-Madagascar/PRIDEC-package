test_that("validate_inputs works in general", {
  input_path <- testthat::test_path("fixtures", "demo_inputs.Rds")
  load(input_path)

  bad_data <- disease_data
  colnames(bad_data)[4] <- "misnamed"

  testthat::expect_true(validate_inputs(config = config,
                          external_data = external_data,
                          disease_data = disease_data,
                          climate_data = climate_data,
                          orgUnit_poly = orgUnit_poly,
                          return_inputs = FALSE))

  testthat::expect_false(validate_inputs(config = config,
                                                 external_data = external_data,
                                                 disease_data = bad_data,
                                                 climate_data = climate_data,
                                                 orgUnit_poly = orgUnit_poly,
                                                 return_inputs = FALSE))
})
