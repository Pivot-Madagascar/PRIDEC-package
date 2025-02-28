test_that("full arimax workflow works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("rain_mm", "temp_c"),
                                                           scaled_vars = NULL,
                                                           graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[8]]

  #only test on three orgUnits (takes some time)
  ex_orgUnits <- c("CSB2 FASINTSARA", "CSB2 MAROHARATRA", "CSB2 IFANADIANA")
  cv_set$analysis <- dplyr::filter(cv_set$analysis, orgUnit %in% ex_orgUnits)
  cv_set$assessment <- dplyr::filter(cv_set$assessment, orgUnit %in% ex_orgUnits)
  #fit
  expect_no_condition(
  test_fit <- fit_arima(cv_set,
                        y_var = "n_case",
                        pred_vars = c("rain_mm", "temp_c"))
  )

  # ----- evaluate -------
  test_eval <- eval_performance(test_fit)
  expect_equal(test_eval$wis, c(10.9910826901975, 12.0375351709772))

  # ----- visualize ------
  multi_plot <- plot_predictions(test_fit)
  expect_contains(class(multi_plot), "ggplot")

  #---- variable investigation --------------
  var_scales <- prep_data(raw_data = demo_malaria,
                          y_var = "n_case",
                          lagged_vars =  c("rain_mm", "temp_c"),
                          scaled_vars = NULL,
                          graph_poly = NULL)$scale_factors
  inv_var_test <- inv_variables_arima(cv_set,
                                      y_var = "n_case",
                                      pred_vars = c("rain_mm", "temp_c"),
                                      var_scales = var_scales)

  expect_equal(colnames(inv_var_test$var_imp), c("variable", "importance"))
  expect_equal(length(inv_var_test$counter_data), 2)
  expect_equal(class(inv_var_test$counter_data[[1]]), "data.frame")

  expect_equal(sum(inv_var_test$var_imp$importance), 1)

  plot_counterfactual(inv_var_test$counter_data)

})

test_that("arimax fits to one orgUnit", {
  demo_df <- data.frame(orgUnit = "orgA",
                        date = seq.Date(as.Date("2018-01-01"), by = "month",
                                        length.out = 51))
  demo_df$y_obs<- (sin(2*pi/12*lubridate::month(demo_df$date))+5)*10 + rnorm(nrow(demo_df), sd = 2)
  demo_df$true_value <- demo_df$y_obs
  demo_df$x1 <- (sin(2*pi/12*lubridate::month(demo_df$date))*3 +rnorm(nrow(demo_df)))
  demo_df$x2 <- (cos(2*pi/12*lubridate::month(demo_df$date))*12 +rnorm(nrow(demo_df)))

  expect_no_condition(
    test_x1 <- fit_arima_OneOrgUnit(train_df = demo_df[1:48,],
                                   test_df = demo_df[49:51,],
                                   pred_vars = c("x1"),
                                   quantile_levels = c(0.2,0.5, 0.643))
  )

  expect_no_condition(
    test_x2 <- fit_arima_OneOrgUnit(train_df = demo_df[1:48,],
                                     test_df = demo_df[49:51,],
                                     pred_vars = c("x1", "x2"),
                                     quantile_levels = 0.5)
  )


})

test_that("internal get_arima_pi works", {
  demo_df <- data.frame(orgUnit = "orgA",
                        date = seq.Date(as.Date("2018-01-01"), by = "month",
                                        length.out = 27))
  demo_df$y_obs<- (sin(2*pi/12*lubridate::month(demo_df$date))+5)*10 + rnorm(nrow(demo_df), sd = 2)
  demo_df$x1 <- (sin(2*pi/12*lubridate::month(demo_df$date))*3 +rnorm(nrow(demo_df)))

  this_y_ts <- stats::ts(data = demo_df$y_obs[1:24],
                         start = c(lubridate::year(min(demo_df$date)),
                                   lubridate::month(min(demo_df$date[1]))),
                         frequency = 12)
  #testing without exogenous variables
  demo_mod <-forecast::auto.arima(this_y_ts)
  demo_pis <- get_arima_pi(demo_mod, quantile_levels = c(0.25, 0.5, 0.75), h =3, xreg = NULL)
  expect_equal(colnames(demo_pis), c("date", "predicted", "quantile_level", "quant_long"))

  #with xreg
  this_x_ts <- stats::ts(data = demo_df$x1[1:24],
                         start = c(lubridate::year(min(demo_df$date)),
                                   lubridate::month(min(demo_df$date[1]))),
                         frequency = 12)
  demo_arimax <- forecast::auto.arima(this_y_ts,
                                      xreg = this_x_ts)
  demo_pi_x <- get_arima_pi(demo_arimax,
                             quantile_levels = c(0.25, 0.5, 0.75),
                             h = 3,
                             xreg = as.matrix(demo_df$x1[25:27]))
  expect_type(demo_pi_x$predicted,"double")
})

test_that("arimax log transform works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("rain_mm", "temp_c"),
                                                           scaled_vars = NULL,
                                                           graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[8]]

  #only test on three orgUnits (takes some time)
  ex_orgUnits <- c("CSB2 IFANADIANA")
  cv_set$analysis <- dplyr::filter(cv_set$analysis, orgUnit %in% ex_orgUnits)
  cv_set$assessment <- dplyr::filter(cv_set$assessment, orgUnit %in% ex_orgUnits)
  cv_clean <- get_cv_subsets(cv_set, y_var = "n_case", pred_vars = c("rain_mm", "temp_c"))
  this_analysis <- cv_clean$analysis
  this_assess <- cv_clean$assess
  #fit
  expect_no_condition({
  fit_nolog <- fit_arima_OneOrgUnit(train_df =cv_clean$analysis,
                                  test_df = cv_clean$assess,
                                  quantile_levels = c(0.25,0.5,0.75),
                                  pred_vars = c("rain_mm", "temp_c"))
  fit_log <-fit_arima_OneOrgUnit(train_df =cv_clean$analysis,
                                 test_df = cv_clean$assess,
                                 quantile_levels = c(0.25,0.5,0.75),
                                 pred_vars = c("rain_mm", "temp_c"),
                                 log_trans = TRUE)
  plot(fit_nolog$predicted, fit_log$predicted)
  eval_performance(fit_nolog)
  eval_performance(fit_log)
  })
})
