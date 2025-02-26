test_that("full inla workflow works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("rain_mm", "temp_c"),
                                                           scaled_vars = NULL,
                                                           graph_poly = demo_polygon)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[6]]
  W_orgUnit <- prep_data(raw_data = demo_malaria,
                             y_var = "n_case",
                             lagged_vars =  c("rain_mm", "temp_c"),
                             scaled_vars = NULL,
                             graph_poly = demo_polygon)$W_graph

  test_fit <- fit_inla(cv_set = cv_set,
                       y_var = "n_case",
                       pred_vars = c("rain_mm", "temp_c"),
                       id_vars = c("orgUnit", "date"),
                       W_orgUnit = W_orgUnit)
  expect_contains(colnames(test_fit), c("observed", "predicted", "quantile_level"))

  expect_no_condition(eval_performance(test_fit))

  #check plot
  plot_predictions(test_fit[test_fit$orgUnit %in% c("CSB2 ATSINDRA", "CSB2 IFANADIANA"),])
  expect_contains(class(plot_predictions(test_fit[test_fit$orgUnit %in% c("CSB2 ATSINDRA", "CSB2 IFANADIANA"),])),
                  "ggplot")

  #---- test variable importance -----
  test_vi <- calc_inla_vi(cv_set = cv_set,
               y_var = "n_case",
               pred_vars = c("rain_mm", "temp_c"),
               id_vars = c("date", "orgUnit"),
               W_orgUnit = W_orgUnit)
  expect_equal(nrow(test_vi), 2)
  expect_equal(sum(test_vi$importance),1)
  expect_true(all(test_vi$importance>=0))

  #---- test counterfactual -------
  test_counter <- create_counterfactual_inla(cv_set = cv_set,
                                             y_var = "n_case",
                                             pred_vars = c("rain_mm", "temp_c"),
                                             id_vars = c("date", "orgUnit"),
                                             W_orgUnit = W_orgUnit,
                                             constant_org = "CSB2 RANOMAFANA",
                                             constant_date = "2018-04-01")
  expect_contains(class(test_counter[[1]]), "data.frame")
  expect_equal(colnames(test_counter[[2]]), c("variable", "var_valuesc", "yhat", "var_value"))

  expect_equal(plot_counterfactual_one(test_counter[[1]], var_label = "orgUnit"),  NULL)

  expect_no_condition(plot_counterfactual_one(test_counter[[4]], var_label = "Rain (mm)"))

  #---- all variable importance together--------##
  inv_var <- inv_variables_inla(cv_set = cv_set,
                                y_var = "n_case",
                                pred_vars = c("rain_mm", "temp_c"),
                                id_vars = c("date", "orgUnit"),
                                W_orgUnit = W_orgUnit,
                                constant_org = "CSB2 RANOMAFANA",
                                constant_date = "2018-04-01",
                                seed = 123,
                                nsims = 5)
  expect_equal(nrow(inv_var$var_imp), 2)
  expect_equal(length(inv_var$counter_data), 5)
})

test_that("inla internal functions work",{

  expect_equal(names(create_inla_setup(hyper_priors = list("prec.unstruct" = c(1, 5e-4),
                                        "prec.spatial" = c(1, 5e-4),
                                        "prec.timerw1" = c(1,0.01)))),
               c("prior_sp", "prior_time", "reff_var", "pi_var"))

  expect_no_condition(get_inla_pi_sample(inla_post_sample = rnorm(1e4,0,1),
                     quantile_levels = c(0.25,0.5,0.75),
                     id_df = data.frame("orgUnit" = "orgA",
                                        date = "2024-01-01")))
  #simple example from INLA example
  n = 10
  x = rnorm(n)
  sd = 0.1
  y = 1+x + rnorm(n,sd=sd)
  res = INLA::inla(y ~ 1 + x, data = data.frame(x,y),
             control.family=list(initial = log(1/sd^2L),fixed=TRUE))

  expect_type(sample_fixed_posteriors(res)$sample, "double")

})


