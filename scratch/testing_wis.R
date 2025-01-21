data(demo_malaria)
data(demo_polygon)

cv_set <- split_cv_rolling(data_to_split = prep_caseData(raw_data = demo_malaria,
                                                         y_var = "n_case",
                                                         lagged_vars =  c("rain_mm", "temp_c"),
                                                         scaled_vars = NULL,
                                                         graph_poly = NULL)$data_prep,
                           month_analysis = 48,
                           month_assess = 3)[[8]]

#only test on one orgUnit
cv_set$analysis <- dplyr::filter(cv_set$analysis, orgUnit == "CSB2 FASINTSARA")
cv_set$assessment <- dplyr::filter(cv_set$assessment, orgUnit == "CSB2 FASINTSARA")
#fit
test_fit <- fit_arima(cv_set,
                        y_var = "n_case",
                        pred_vars = c("rain_mm", "temp_c"))

calc_is <- function(obs, up, lw, alpha){
  ((up - lw) + 2/alpha*(lw-obs)*(obs<lw) + 2/alpha*(obs-up) * (obs>up))*alpha/2
}

dplyr::filter(test_fit, orgUnit == "CSB2 FASINTSARA", date == as.Date("2020-05-01"),
              quantile_level %in% c(0.7,0.3))
calc_is(obs = 33, up = 72.22, lw = 46.89, alpha = 0.6)
calc_is(obs = 33, up = 50.22, lw = 20, alpha = 0.4)
scoringutils:::wis(33, matrix(c(72.22,46.89), ncol = 2), quantile_level = c(0.7,0.3), weigh = FALSE)

one_pred <- dplyr::filter(test_fit, orgUnit == "CSB2 FASINTSARA", date == as.Date("2020-05-01"))
scoringutils:::wis(observed = one_pred$observed[1], predicted = matrix(one_pred$predicted, nrow = 1),
                  quantile_level = one_pred$quantile_level, weigh = TRUE)

  mean(na.rm = TRUE)

get_wis <- function(pred_df){
  if(nrow(pred_df)<1){
    return(as.numeric(NA))
  }
  scoringutils::wis(observed = unique(pred_df$observed),
                    predicted = matrix(pred_df$predicted, nrow = 1),
                    quantile_level = pred_df$quantile_level, weigh = TRUE,
                    separate_results = TRUE) |>
    unlist()
}

one_org <- filter(test_fit, orgUnit == "CSB2 FASINTSARA")
org_list <- split(one_org, ~ one_org$date + one_org$dataset)
split(one_org, ~ one_org$date + one_org$dataset) |>
  purrr::map(\(x) get_wis(pred_df = x)) |>
  unlist()

get_wis(org_list[[1]])

int_df <- test_fit |>
  tidyr::nest(.by = c("date", "dataset", "orgUnit")) |>
  mutate(wis_list = lapply(data, get_wis)) |>
  select(-data)

scores <- int_df |>
  pull(wis_list) |>
  purrr::map(\(x) unlist(x)) |>
  bind_rows() |>
  bind_cols(select(int_df, all_of(c("date", "dataset", "orgUnit")))) |>
  group_by(.data$orgUnit, .data$dataset) |>
  summarise(wis = mean(wis, na.rm = TRUE),
            dispersion = mean(dispersion, na.rm = TRUE))

test_fit |>
  scoringutils::as_forecast_quantile(forecast_unit = c("orgUnit", "date", "dataset"),
                                     quantile_level = "quantile_level") |>

  scoringutils::score() |>
  scoringutils::summarise_scores(by = c("dataset", "orgUnit")) |>
  select(wis, dispersion)
