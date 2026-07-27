test_that("impute seasonal on normal data", {
  data_vector <- 15 + 10 * cos(2 * pi * (rep(c(1:12),4) - 7) / 12) + rnorm(48, mean = 0, sd = 1.5)
  data_vector[sample(1:48,24)] <- NA
  expect_no_error(impute_seasonal(data_vector))
})


test_that("impute seasonal on constant data", {
  data_vector <- rep(1,48)
  data_vector[sample(1:48,24)] <- NA
  expect_no_error(impute_seasonal(data_vector))
  expect_equal(impute_seasonal(data_vector), rep(1,48))
})

test_that("impute seasonal on missing data", {
  data_vector <- rep(NA_real_,48)
  expect_equal(impute_seasonal(data_vector), rep(NA_real_, 48))
})


test_that("impute seasonal on short data gives warning", {
  data_vector <- 15 + 10 * cos(2 * pi * (1:12 - 7) / 12) + rnorm(12, mean = 0, sd = 1.5)
  data_vector[sample(1:12,4)] <- NA
  expect_warning(impute_seasonal(data_vector))
})

test_that("impute seasonal on sparse data", {
  data_vector <- rep(0,48)
  data_vector[sample(1:48,3)] <- rnorm(3, 0, sd = 0.01)
  data_vector[sample(1:48, 25)] <- NA
  expect_no_error(impute_seasonal(data_vector))
})
