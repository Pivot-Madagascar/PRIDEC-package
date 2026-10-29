
#------- create supporting assets ------------------------#

make_glm_df <- function(n = 120){
  set.seed(10)
  df <- data.frame(orgUnit = factor(rep(c("orgA", "orgB", "orgC"), length.out = n)),
                   month_season = factor(rep(1:12, length.out = n)),
                   x1 = rnorm(n),
                   x2 = rnorm(n))
  df$y_obs <- rnbinom(n, mu = exp(1 + 0.3*df$x1), size = 5)
  df
}


#--------------model checks ------------------


test_that("screen_glm_vars removes static and collinear variables", {
  df <- make_glm_df()
  df$x3 <- df$x1*2 + 1      #perfectly collinear with x1
  df$x4 <- 1                #static
  df$x5 <- df$x1 + df$x2    #linear combination
  df$f1 <- factor("a")      #factor with a single level

  expect_equal(screen_glm_vars(df, c("orgUnit", "month_season", "x1", "x2")),
               c("orgUnit", "month_season", "x1", "x2"))
  expect_equal(screen_glm_vars(df, c("orgUnit", "month_season", "x1", "x2", "x3", "x4", "x5", "f1")),
               c("orgUnit", "month_season", "x1", "x2"))
  #earlier variables preferentially kept
  expect_equal(screen_glm_vars(df, c("orgUnit", "month_season", "x3", "x1")),
               c("orgUnit", "month_season", "x3"))
  #fixed variables are never dropped
  expect_equal(screen_glm_vars(df, c("orgUnit", "month_season")),
               c("orgUnit", "month_season"))
  #more columns than rows
  expect_equal(screen_glm_vars(df[1:15,], c("orgUnit", "month_season", "x1")),
               c("orgUnit", "month_season"))
})

test_that("retry_glm_nb keeps all variables when the model converges", {
  df <- make_glm_df()
  expect_no_message(
    res <- retry_glm_nb(df, pred_vars = c("x1", "x2"))
  )
  expect_equal(res$vars, c("orgUnit", "month_season", "x1", "x2"))
  expect_s3_class(res$model, "negbin")
})

test_that("retry_glm_nb drops the last variable when the model errors", {
  df <- make_glm_df()
  df$x_bad <- NA_real_ #all rows removed, so glm.nb errors

  expect_message(
    res <- retry_glm_nb(df, pred_vars = c("x1", "x_bad")),
    "Dropping x_bad"
  )
  expect_equal(res$vars, c("orgUnit", "month_season", "x1"))
  expect_s3_class(res$model, "negbin")
})

test_that("retry_glm_nb treats convergence warnings as failures", {
  df <- make_glm_df()
  conv_msgs <- c("iteration limit reached", "alternation limit reached",
                 "glm.fit: algorithm did not converge")

  for(msg in conv_msgs){
    call_n <- 0
    #first fit warns about convergence, second fit is fine
    testthat::local_mocked_bindings(
      glm.nb = function(formula, data, ...){
        call_n <<- call_n + 1
        if(call_n == 1) warning(msg)
        stats::glm(formula, data = data, family = stats::poisson())
      },
      .package = "MASS"
    )
    expect_message(
      res <- retry_glm_nb(df, pred_vars = c("x1", "x2")),
      "Dropping x2"
    )
    expect_equal(call_n, 2)
    expect_equal(res$vars, c("orgUnit", "month_season", "x1"))
  }
})

test_that("retry_glm_nb passes on unrelated warnings without dropping variables", {
  df <- make_glm_df()
  testthat::local_mocked_bindings(
    glm.nb = function(formula, data, ...){
      warning("some other warning")
      stats::glm(formula, data = data, family = stats::poisson())
    },
    .package = "MASS"
  )
  expect_warning(
    res <- retry_glm_nb(df, pred_vars = c("x1", "x2")),
    "some other warning"
  )
  expect_equal(res$vars, c("orgUnit", "month_season", "x1", "x2"))
})

test_that("retry_glm_nb returns NULL once only the fixed variables are left", {
  df <- make_glm_df()
  call_n <- 0
  testthat::local_mocked_bindings(
    glm.nb = function(formula, data, ...){
      call_n <<- call_n + 1
      warning("iteration limit reached")
    },
    .package = "MASS"
  )
  expect_message(
    res <- retry_glm_nb(df, pred_vars = c("x1", "x2")),
    "no optional predictors remain"
  )
  expect_null(res)
  #fit with x1 + x2, then x1; the model with only the fixed variables is not attempted
  expect_equal(call_n, 2)
})

test_that("fit_glm_nb returns NULL if the model cannot be fit", {
  data(demo_malaria)
  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                       y_var = "n_case",
                                                       lagged_vars =  c("rain_mm", "temp_c"),
                                                       scaled_vars = NULL,
                                                       graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[3]]
  testthat::local_mocked_bindings(
    glm.nb = function(formula, data, ...) stop("cannot fit"),
    .package = "MASS"
  )

  suppressMessages({
    fit <- fit_glm_nb(cv_set, y_var = "n_case", id_vars = c("date", "orgUnit"),
                      pred_vars = c("rain_mm", "temp_c"))
    fit_mod <- fit_glm_nb(cv_set, y_var = "n_case", id_vars = c("date", "orgUnit"),
                          pred_vars = c("rain_mm", "temp_c"), return_model = TRUE)
  })
  expect_null(fit)
  expect_equal(fit_mod, list(model = NULL, predictions = NULL))
})


# --------------- full workflows--------------------------

test_that("full glm_nb workflow works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("rain_mm", "temp_c"),
                                                           scaled_vars = NULL,
                                                           graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[16]]

  expect_no_condition(
    test_fit <- fit_glm_nb(cv_set,
                          y_var = "n_case",
                          id_vars = c("date", "orgUnit"),
                          pred_vars = c("rain_mm", "temp_c"))
  )
  #check performance
  expect_equal(eval_performance(test_fit)$wape, c(0.532286544683763, 0.72950493946477))
  #check plot
  expect_contains(class(plot_predictions(test_fit[test_fit$orgUnit %in% c("CSB2 ATSINDRA", "CSB2 IFANADIANA"),])),
                  "ggplot")
})

test_that("glm.nb variable exploration works", {
  data(demo_malaria)
  data(demo_polygon)

  cv_set <- split_cv_rolling(data_to_split = prep_data(raw_data = demo_malaria,
                                                           y_var = "n_case",
                                                           lagged_vars =  c("rain_mm", "temp_c"),
                                                           scaled_vars = NULL,
                                                           graph_poly = NULL)$data_prep,
                             month_analysis = 48,
                             month_assess = 3)[[3]]


  test_inv <- inv_variables_glm_nb(cv_set = cv_set,
                                   y_var = "n_case",
                                   id_vars = c("orgUnit", "date"),
                                   pred_vars = c("rain_mm", "temp_c"),
                                   nsim = 50)

  expect_type(test_inv$varImp$importance, "double")
  expect_equal(length(test_inv$counter_data), 4)
  expect_no_condition(plot_counterfactual_one(test_inv$counter_data[[3]], var_label = "rain"))
  expect_equal(sum(test_inv$varImp$importance),1)
  expect_true(all(test_inv$varImp$importance>=0))

})



