# --------------------supporting functions ------

make_preds <- function(predicted, quant_long = "quant_0.5"){
  data.frame(orgUnit = "orgA",
             date = as.Date("2020-01-01"),
             quant_long = quant_long,
             predicted = predicted)
}

make_arima_df <- function(n = 51, seed = 3){
  set.seed(seed)
  df <- data.frame(orgUnit = "orgA",
                   date = seq.Date(as.Date("2018-01-01"), by = "month", length.out = n))
  df$y_obs <- (sin(2*pi/12*lubridate::month(df$date)) + 5)*10 + rnorm(n, sd = 2)
  df$x1 <- rnorm(n)
  df
}

# ----------------full ensemble workflow ------------------------

test_that("ensemble forecast works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                       y_var = "n_case",
                                                       lagged_vars =  c("rain_mm", "temp_c"),
                                                       scaled_vars = NULL,
                                                       graph_poly = demo_polygon)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[14]]
  W_orgUnit <- prep_data(raw_data = demo_malaria,
                         y_var = "n_case",
                         lagged_vars =  c("rain_mm", "temp_c"),
                         scaled_vars = NULL,
                         graph_poly = demo_polygon)$W_graph
  #create configs for each model
  inla_configs <- list(reff_var = NULL, pred_vars = c("rain_mm", "temp_c"),
                       hyper_priors = INLA_HYPER_DEFAULT,
                       W_orgUnit = W_orgUnit, sample_pi = FALSE,
                       weight = 0.5)
  glm_nb_configs <- list(pred_vars = c("rain_mm", "temp_c"),
                         weight = 0.1)
  ranger_configs <- list(pred_vars = c("rain_mm", "temp_c"),
                         hyper_control = RANGER_HYPER_DEFAULT,
                         weight = 0.05)
  arimax_configs <- list(pred_vars = c("rain_mm", "temp_c"),
                         log_trans = TRUE,
                         weight = 0.2)
  naive_configs <- list(group_vars = c("month_season", "orgUnit"),
                        weight = 0.02)

  #run the forecast
  stack_forecast <- ensemble_forecast(cv_set = cv_set,
                                      y_var = "n_case",
                                      id_vars = c("orgUnit", "date"),
                                      quantile_levels = c(0.25,0.5,0.75),
                                      inla_configs = inla_configs,
                                      glm_nb_configs = glm_nb_configs,
                                      ranger_configs = ranger_configs,
                                      arimax_configs = arimax_configs,
                                      naive_configs = naive_configs)
  expect_contains(colnames(stack_forecast), c("orgUnit", "predicted", "observed", "quantile_level"))

  # expect_error(plot_predictions(stack_forecast, quantile_ribbon = c(0.025, 0.975)), regexp = NULL)

  plot_predictions(stack_forecast, quantile_ribbon = c(0.25, 0.75))

  eval_performance(stack_forecast)

})

# ---- fit_ensemble_member: prediction feasibility ----------------------------

test_that("valid predictions are kept and labelled with model and weight", {
  out <- expect_no_warning(
    fit_ensemble_member("naive", "NAIVE", weight = 0.3,
                        fit_fun = function() make_preds(5), max_allowed = 100)
  )
  expect_equal(out$model, "naive")
  expect_equal(out$weight, 0.3)
  expect_equal(out$predicted, 5)
})

test_that("models with predictions above the ceiling are dropped", {
  expect_warning(
    out <- fit_ensemble_member("naive", "NAIVE", weight = 1,
                               fit_fun = function() make_preds(101), max_allowed = 100),
    "NAIVE model removed from the ensemble.*higher than the maximum allowed"
  )
  expect_null(out)

  #exactly at the ceiling is allowed
  expect_no_warning(
    out <- fit_ensemble_member("naive", "NAIVE", weight = 1,
                               fit_fun = function() make_preds(100), max_allowed = 100)
  )
  expect_false(is.null(out))
})

test_that("only the median is checked against the ceiling when it exists", {
  #extreme upper quantile does not drop the model
  preds <- rbind(make_preds(10, "quant_0.5"), make_preds(1e6, "quant_0.99"))
  expect_no_warning(
    out <- fit_ensemble_member("naive", "NAIVE", weight = 1,
                               fit_fun = function() preds, max_allowed = 100)
  )
  expect_equal(nrow(out), 2)

  #extreme median does
  preds <- rbind(make_preds(1e6, "quant_0.5"), make_preds(10, "quant_0.99"))
  expect_warning(
    out <- fit_ensemble_member("naive", "NAIVE", weight = 1,
                               fit_fun = function() preds, max_allowed = 100),
    "removed from the ensemble"
  )
  expect_null(out)

  #without a median, all rows are checked
  preds <- rbind(make_preds(10, "quant_0.25"), make_preds(1e6, "quant_0.99"))
  expect_warning(
    out <- fit_ensemble_member("naive", "NAIVE", weight = 1,
                               fit_fun = function() preds, max_allowed = 100),
    "removed from the ensemble"
  )
  expect_null(out)
})

test_that("models with infinite predictions are dropped even without a ceiling", {
  expect_warning(
    out <- fit_ensemble_member("glm_nb", "GLM", weight = 1,
                               fit_fun = function() make_preds(c(1, Inf), c("quant_0.5", "quant_0.99"))),
    "GLM model removed from the ensemble.*infinite"
  )
  expect_null(out)
})

test_that("the ceiling check never aborts, even if abort_on_error = TRUE", {
  expect_warning(
    out <- fit_ensemble_member("naive", "NAIVE", weight = 1, abort_on_error = TRUE,
                               fit_fun = function() make_preds(1e6), max_allowed = 100),
    "removed from the ensemble"
  )
  expect_null(out)
})

# ---- fit_ensemble_member: failed fits ---------------------------------------

test_that("models that error, return NULL, or return nothing usable are dropped", {
  expect_warning(
    out <- fit_ensemble_member("inla", "INLA", weight = 1,
                               fit_fun = function() stop("boom")),
    "INLA model removed from the ensemble: it failed with error message: boom"
  )
  expect_null(out)

  expect_warning(
    out <- fit_ensemble_member("inla", "INLA", weight = 1, fit_fun = function() NULL),
    "did not fit correctly"
  )
  expect_null(out)

  expect_warning(
    out <- fit_ensemble_member("inla", "INLA", weight = 1,
                               fit_fun = function() make_preds(NA_real_)),
    "no valid predictions"
  )
  expect_null(out)

  expect_warning(
    out <- fit_ensemble_member("inla", "INLA", weight = 1,
                               fit_fun = function() make_preds(1)[0,]),
    "no valid predictions"
  )
  expect_null(out)

  expect_warning(
    out <- fit_ensemble_member("inla", "INLA", weight = 1, fit_fun = function() "not a df"),
    "no valid predictions"
  )
  expect_null(out)
})

test_that("abort_on_error re-throws model errors", {
  expect_error(
    fit_ensemble_member("inla", "INLA", weight = 1, abort_on_error = TRUE,
                        fit_fun = function() stop("boom")),
    "INLA model failed with error message:.*boom"
  )
})

# ---- ensemble_forecast ------------------------------------------------------

test_that("ensemble_forecast drops implausible models and errors if none remain", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                       y_var = "n_case",
                                                       lagged_vars = c("rain_mm", "temp_c"),
                                                       scaled_vars = NULL,
                                                       graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[8]]
  naive_configs <- list(group_vars = c("month_season", "orgUnit"), weight = 1)

  #default ceiling does not remove a sensible naive model
  expect_no_warning(
    ens <- ensemble_forecast(cv_set = cv_set, y_var = "n_case",
                             id_vars = c("orgUnit", "date"),
                             quantile_levels = c(0.25, 0.5, 0.75),
                             naive_configs = naive_configs)
  )
  expect_contains(colnames(ens), c("orgUnit", "predicted", "observed", "quantile_level"))

  #an impossibly low ceiling drops every model
  expect_error(
    suppressWarnings(
      ensemble_forecast(cv_set = cv_set, y_var = "n_case",
                        id_vars = c("orgUnit", "date"),
                        quantile_levels = c(0.25, 0.5, 0.75),
                        naive_configs = naive_configs,
                        max_pred_multiplier = 1e-6)
    ),
    "All models in the ensemble failed"
  )
  expect_warning(
    try(ensemble_forecast(cv_set = cv_set, y_var = "n_case",
                          id_vars = c("orgUnit", "date"),
                          quantile_levels = c(0.25, 0.5, 0.75),
                          naive_configs = naive_configs,
                          max_pred_multiplier = 1e-6), silent = TRUE),
    "NAIVE model removed from the ensemble"
  )
})

