test_that("data preparation function works", {
  data(demo_malaria)
  data(demo_polygon)

  test_prep <- prep_data(raw_data = demo_malaria,
                             y_var = "n_case",
                             lagged_vars =  c("pev", "rain_mm", "temp_c"),
                             scaled_vars = c("wealth_index", "elevation",
                                             "LLIN_use", "time_to_district", "LLIN_wane"),
                             graph_poly = demo_polygon)

  expect_type(test_prep$W_graph, c("double"))
  expect_s3_class(test_prep$data_prep, "data.frame")
})

test_that("seasonal imputation works", {
  fake_df <- expand.grid(orgUnit = LETTERS[1:4],
                         date = seq.Date(as.Date("2018-01-01"), by = "month",
                                         length.out = 36))
  fake_df$temp_c <- 4*sin(lubridate::month(fake_df$date))+ rnorm(nrow(fake_df))
  fake_df$tempNA <- fake_df$temp_c
  fake_df$tempNA[sample(1:nrow(fake_df), 0.3*nrow(fake_df))] <- NA
  fake_df$tempNA2 <- fake_df$tempNA

  fixed_df <- fill_seasonal(fake_df, data_col = "tempNA", group_col = "orgUnit")

  ggplot2::ggplot(fixed_df, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = temp_c), col = "blue") +
    ggplot2::geom_point(ggplot2::aes(y = tempNA2), size =2) +
    ggplot2::geom_point(ggplot2::aes(y = tempNA), size = 1, color = "red") +
    ggplot2::geom_line(ggplot2::aes(y = tempNA), col = "red") +
    ggplot2::facet_wrap(~orgUnit)

  expect_equal(sum(is.na(fixed_df$tempNA)), 0)
})
