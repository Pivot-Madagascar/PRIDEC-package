#' Evaluate performance on a cv_set
#' @param pred_intervals data.frame of prediction intervals, output from `fit` step.
#'   Must include: orgUnit, date, dataset, observed, predicted, quant_long, quantile_level
#'
#' @details
#' Point-forecast scores are calculated for each orgUnit and dataset using the
#' median forecast (`quant_0.5`) and rows where `observed` is not missing.
#' They are then averaged across orgUnits within each dataset (ignoring `NA`s),
#' alongside the weighted interval score (`wis`) and its `dispersion` component
#' from, which use the full quantiles.
#'
#' \describe{
#'   \item{wape}{Weighted absolute percentage error:
#'     `sum(|predicted - observed|) / sum(|observed|)`. Total absolute error
#'     scaled by total observed cases, so months with more cases carry more
#'     weight. Unlike a mean percentage error, it is defined when individual
#'     observations are zero (but not when all observations are zero).}
#'   \item{mean_ae_log}{Absolute error on the log scale:
#'     `|log(predicted + 1) - log(observed + 1)|`. The +1 avoids `log(0)`.
#'     Measures relative (multiplicative) error, so it is less dominated by
#'     large values than `mae`. Note that it is currently aggregated with the
#'     median across dates, not the mean, despite its name.}
#'   \item{med_ae}{Median absolute error, `median(|predicted - observed|)`,
#'     in the original units. Robust to a few very poor forecasts.}
#'   \item{mae}{Mean absolute error, `mean(|predicted - observed|)`, in the
#'     original units. Sensitive to large misses.}
#'   \item{prop_over}{Proportion of forecasts where `predicted > observed`
#'     (overprediction).}
#'   \item{prop_under}{Proportion of forecasts where `predicted < observed`
#'     (underprediction). `prop_over + prop_under` is below 1 when some
#'     forecasts exactly match the observation. A well-calibrated model
#'     should have both near 0.5.}
#'   \item{sp_rho}{Spearman rank correlation between observed and predicted.
#'     Measures whether the forecast gets the relative ordering of high and low
#'     months right, regardless of bias or scale. `NA` when either series is
#'     constant or there are too few observations.}
#' }
#'
#' @returns dataset of performance metrics for assessment and anlalysis datasets for that cv_set
#' @export
eval_performance <- function(pred_intervals){

  #estimate med_ae_log as log(x+1) to help with zeros
  #estimate wape (weighted average of the mean absolute error)
  #proportion under and proportion over
  med_errs <- pred_intervals |>
    dplyr::filter(.data$quant_long == "quant_0.5", !is.na(.data$observed)) |>
    dplyr::summarise(wape = sum(abs(.data$predicted-.data$observed))/sum(abs(.data$observed)),
                     mean_ae_log = mean(abs((log(.data$predicted+1)-log(.data$observed+1)))),
                     med_ae = median(abs(.data$predicted-.data$observed)),
                     mae = mean(abs(.data$predicted-.data$observed)),
                     prop_over = mean(.data$predicted>.data$observed),
                     prop_under = mean(.data$predicted<.data$observed),
                     sp_rho = suppressWarnings(cor(.data$observed, .data$predicted, use = 'pairwise.complete.obs',
                                  method = "spearman")),
              .by = c("orgUnit", "dataset"))

  raw_scores <- get_wis(pred_intervals) |>
    dplyr::right_join(med_errs, by = c("dataset", "orgUnit"))


  #mean of scores across all OrgUnits/forecasts
  score_summary <- raw_scores |>
    dplyr::summarise(dplyr::across(c("wis", "mae", "med_ae", "mean_ae_log", "wape",
                          "dispersion",
                          "sp_rho", "prop_over", "prop_under"),
                          ~ mean(.x, na.rm = TRUE)),
                     .by = "dataset")

  return(score_summary)

}

#' Calculate Weighted Interval Score
#'
#' Internal function to use `wis` from `scoringutils`
#' @param pred_df data.frame of predictions included observed, predicted and quantile_level
get_wis <- function(pred_df){
  #make data.frame wider
  df_wide <- dplyr::select(pred_df, -all_of("quantile_level")) |>
    tidyr::pivot_wider(id_cols = c("orgUnit", "date", "dataset", "observed"),
                                names_from = "quant_long",
                                values_from = "predicted")

  scoringutils::wis(observed = df_wide$observed,
                    # predicted = as.matrix(dplyr::select(df_wide, starts_with("quant"))),
                    predicted = as.matrix(df_wide[, startsWith(colnames(df_wide), "quant")]),
                    quantile_level = as.numeric(gsub("quant_", "", colnames(df_wide)[startsWith(colnames(df_wide), "quant")])),
                    weigh = TRUE,
                    separate_results = TRUE,
                    na.rm = TRUE) |>
    dplyr::bind_cols() |>
    dplyr::bind_cols(df_wide[,c("orgUnit", "dataset")]) |>
    dplyr::summarise(wis = mean(.data$wis, na.rm = TRUE),
              dispersion = mean(.data$dispersion, na.rm = TRUE),
              .by = c("orgUnit", "dataset"))
}

