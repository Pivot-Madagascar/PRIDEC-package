test_that("plotting predictions", {
  demo_data <- create_demo_preds(n_orgUnit = 4,
                                 n_months = 24)

expect_no_condition(plot_predictions(demo_data))

})
