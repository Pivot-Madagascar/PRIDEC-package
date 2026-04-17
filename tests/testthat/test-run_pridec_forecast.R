test_that("pridec forecast works", {
  input_path <- testthat::test_path("fixtures", "demo_inputs.Rds")
  load(input_path)

  #process inputs
  input_list <- validate_inputs(config = config,
                                external_data = external_data,
                                disease_data = disease_data,
                                climate_data = climate_data,
                                orgUnit_poly = orgUnit_poly,
                                return_inputs = TRUE)
  output_dir <- testthat::test_path("examples", "output")
  dir.create(output_dir)

  expect_true(run_pridec_forecast(inputs = input_list, output_dir = output_dir))
  #check files are there
  expect_true(all(c("config.json", "forecast.json", "input_data.json", "input_data.RData",
                "polygon.geojson") %in% list.files(output_dir)))


  #create report
  expect_true(create_forecast_report(report_dir = output_dir, quiet = TRUE))

  expect_true("forecast_report.html" %in% list.files(output_dir))

  unlink(output_dir, recursive = TRUE)
})
