#' Evaluate performance on a cv_set
#' @param pred_intervals data.frame of prediction intervals, output from `fit` step.
#'   Must include: orgUnit, date, dataset, observed, predicted, quant_long, quantile
#' @returns dataset of performance metrics for assessment and anlalysis datasets for that cv_set
eval_performance <- function(pred_intervals){

  #estimate med_ae_log as log(x+1) to help with zeros
  #estimate wape (weighted average of the mean absolute error)
  #proportion under and proportion over
  med_errs <- pred_intervals |>
    dplyr::filter(.data$quant_long == "quant_0.5", !is.na(.data$observed)) |>
    dplyr::summarise(wape = sum(abs(.data$predicted-.data$observed))/sum(abs(.data$observed)),
                     mean_ae_log = median(abs((log(.data$predicted+1)-log(.data$observed+1)))),
                     med_ae = median(abs(.data$predicted-.data$observed)),
                     mae = mean(abs(.data$predicted-.data$observed)),
                     prop_over = mean(.data$predicted>.data$observed),
                     prop_under = mean(.data$predicted<.data$observed),
                     sp_rho = cor(.data$observed, .data$predicted, use = 'pairwise.complete.obs',
                                  method = "spearman"),
              .by = c("orgUnit", "dataset"))

  raw_scores <- pred_intervals |>
    scoringutils::as_forecast_quantile(forecast_unit = c("orgUnit", "date", "dataset"),
                                       quantile_level = "quantile_level") |>

    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("dataset", "orgUnit"))


  raw_scores <- raw_scores |>
    dplyr::right_join(med_errs, by = c("dataset", "orgUnit"))

  #mean of scores across all OrgUnits/forecasts
  score_summary <- raw_scores |>
    dplyr::group_by(.data$dataset) |>
    dplyr::summarise_at(c("wis", "mae", "med_ae", "mean_ae_log", "wape",
                          "bias", "dispersion",
                          "sp_rho", "prop_over", "prop_under"),
                        .funs = mean, na.rm = TRUE) |>
    dplyr::ungroup()

  return(score_summary)

}
