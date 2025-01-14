#' Evaluate performance on a cv_set
#' @param pred_intervals data.frame of prediction intervals, output from `fit` step.
#'   Must include: orgUnit, date, dataset, observed, predicted, quant_long, quantile

eval_performance <- function(pred_intervals){

  #estimate mae_log as log(x+1) to help with zeros
  #estimate wape (weighted average of the mean absolute error)
  #proportion under and proportion over
  med_errs <- pred_intervals |>
    dplyr::filter(quant_long == "quant_0.5", !is.na(observed)) |>
    dplyr::summarise(wape = sum(abs(predicted-observed))/sum(observed),
                     mae_log = mean(abs((log(predicted+1)-log(observed+1)))),
                     mae = mean(abs(predicted-observed)),
                     prop_over = mean(predicted>observed),
                     prop_under = mean(predicted<observed),
              .by = c("orgUnit", "dataset"))

  sp_rho <- pred_intervals |>
    dplyr::filter(quant_long == "quant_0.5") |>
    #if all predictions are the same value, will return NA
    dplyr::summarise(sp_rho = cor(true_value, prediction, use = 'pairwise.complete.obs',
                              method = "spearman"),
              .by = c("orgUnit", "dataset"))


  raw_scores <- pred_intervals |>
    scoringutils::as_forecast_quantile(forecast_unit = c("orgUnit", "date", "dataset"),
                                       quantile_level = "quantile_level") |>

    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("dataset", "orgUnit"))


  if(!("analysis" %in% raw_scores$dataset)){
    analysis_scores <- pred_intervals |>
      filter(dataset == "analysis") |>
      summarise(ae_median = mean(abs(observed - predicted), na.rm = TRUE),
                .by = c("orgUnit", "dataset"))

    raw_scores <- bind_rows(filter(raw_scores, dataset == "assess"),
                            analysis_scores)
  }

  raw_scores <- raw_scores |>
    dplyr::left_join(mean_y, by = c("dataset", "orgUnit")) |>
    dplyr::mutate(mase = ae_median/mean_y) |>
    dplyr::left_join(sp_rho, by = c("dataset", "orgUnit")) |>
    dplyr::left_join(prop_ou, by = c("dataset", "orgUnit"))

  #mean of scores across all OrgUnits
  score_summary <- raw_scores |>
    dplyr::group_by(dataset) |>
    dplyr::summarise_at(c("wis", "mae", "bias", "dispersion", "mase",
                          "sp_rho", "prop_over", "prop_under"),
                        .funs = mean, na.rm = TRUE) |>
    dplyr::ungroup()

}
