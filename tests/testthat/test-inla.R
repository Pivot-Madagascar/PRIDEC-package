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
                       W_orgUnit = W_orgUnit,
                       verbose = FALSE,
                       n_cores = 6)
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

test_that("inla works without spatial structure",{
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                       y_var = "n_case",
                                                       lagged_vars =  c("rain_mm", "temp_c"),
                                                       scaled_vars = NULL,
                                                       graph_poly = demo_polygon)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[6]]

  test_fit <- fit_inla(cv_set = cv_set,
                       y_var = "n_case",
                       pred_vars = c("rain_mm", "temp_c"),
                       id_vars = c("orgUnit", "date"),
                       W_orgUnit = NULL)
  expect_contains(colnames(test_fit), c("observed", "predicted", "quantile_level"))

  expect_no_condition(eval_performance(test_fit))
})

test_that("inla internal functions work",{

  expect_equal(names(create_inla_setup(hyper_priors = list("prec.spatial" = c(1, 0.01),
                                        "phi.spatial" = c(0.5,0.5),
                                        "prec.timerw1" = c(1,0.01)), W_orgUnit = NULL)),
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

test_that("diagnose_inla_data detects structural and predictor problems", {
  set.seed(42)
  orgs <- paste0("org", 1:4)
  make_data <- function(){
    d <- expand.grid(org_ID = orgs, month_season = 1:12, stringsAsFactors = FALSE)
    d$x1 <- rnorm(nrow(d))
    d$x2 <- rnorm(nrow(d))
    d
  }
  # path graph: org1 - org2 - org3 - org4
  W <- matrix(0, 4, 4, dimnames = list(orgs, orgs))
  W[cbind(1:3, 2:4)] <- 1
  W <- W + t(W)

  diagnose <- function(...) suppressMessages(diagnose_inla_data(..., pred_vars = c("x1", "x2")))

  #clean data has no issues
  clean <- diagnose(data = make_data(), W_orgUnit = W)
  expect_length(clean[[1]], 0)
  expect_length(clean[[2]], 0)

  #works without a graph
  expect_length(diagnose(data = make_data(), W_orgUnit = NULL)[[1]], 0)

  #zero variance predictor
  d <- make_data()
  d$x1 <- 1
  res <- diagnose(data = d, W_orgUnit = W)
  expect_named(res[[2]], "x1")

  #perfectly collinear predictors -> rank deficient design matrix
  d <- make_data()
  d$x2 <- 2 * d$x1
  res <- diagnose(data = d, W_orgUnit = W)
  expect_true("design_matrix_rank_deficient" %in% names(res[[1]]))

  #org in data but not in graph
  d <- make_data()
  d$org_ID[d$org_ID == "org4"] <- "org5"
  res <- diagnose(data = d, W_orgUnit = W)
  expect_equal(res[[1]]$missing_from_graph, "org5")

  #disconnected graph (islands) and zero-degree node
  W_island <- W
  W_island[3:4, ] <- 0
  W_island[, 3:4] <- 0
  res <- diagnose(data = make_data(), W_orgUnit = W_island)
  expect_true("islands" %in% names(res[[1]]))
  expect_true(all(c("org3", "org4") %in% res[[1]]$zero_degree_nodes))

  #thin temporal coverage and singleton orgs
  d <- make_data()
  d <- d[!(d$org_ID == "org1" & d$month_season > 2), ]
  res <- diagnose(data = d, W_orgUnit = W)
  expect_equal(unname(res[[1]]$thin_month_coverage["org1"]), 2)
  d <- d[!(d$org_ID == "org2" & d$month_season > 1), ]
  res <- diagnose(data = d, W_orgUnit = W)
  expect_equal(res[[1]]$singleton_orgs, "org2")

  #unused factor levels
  d <- make_data()
  d$org_ID <- factor(d$org_ID, levels = c(orgs, "org_unused"))
  res <- diagnose(data = d, W_orgUnit = NULL)
  expect_equal(res[[1]]$unused_levels_org_ID, "org_unused")
})

test_that("run_inla_config skips fitting when diagnostics find fatal problems", {
  set.seed(42)
  d <- expand.grid(org_ID = paste0("org", 1:3), month_season = 1:12, stringsAsFactors = FALSE)
  d$x1 <- 1 #zero variance
  d$x2 <- rnorm(nrow(d))
  d$y_obs <- rpois(nrow(d), 5)
  d$idx <- seq_len(nrow(d))
  out <- suppressMessages(run_inla_config(data = d, pred_vars = c("x1", "x2"), verbose = FALSE))
  expect_null(out)
})
