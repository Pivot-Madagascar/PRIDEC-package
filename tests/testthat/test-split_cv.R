test_that("cv splits follows correct year and month splits", {
  data_to_split <- expand.grid(orgUnit = LETTERS[1:4],
                               date = seq.Date(as.Date("2012-01-01"), by = "month",
                                               length.out = 24))
  data_to_split$y_var <- runif(nrow(data_to_split),0, 100)

  expect_length(split_cv_rolling(data_to_split, month_analysis = 12, month_assess = 2),
               9)
  expect_equal(nrow(split_cv_rolling(data_to_split, month_analysis = 12, month_assess = 5)[[1]]$assessment),
               20)
  expect_error(split_cv_rolling(data_to_split, month_analysis, 40, month_assess = 10))
})


test_that("train/test split works",{
  data_to_split <- expand.grid(orgUnit = LETTERS[1:4],
                               date = seq.Date(as.Date("2012-01-01"), by = "month",
                                               length.out = 36))
  data_to_split$y_var <- runif(nrow(data_to_split),0, 100)
  cv_data <- split_cv_rolling(data_to_split, month_analysis = 12, month_assess = 2)

  expect_named(split_stratified(cv_data), c("train_data", "test_data"))
  expect_length(split_stratified(cv_data)$test_data, 7)
})
