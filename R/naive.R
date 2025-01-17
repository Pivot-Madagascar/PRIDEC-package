#' Fit a naive model to a cv_set
#'
#' @param cv_set list object containing analysis and assessment data.frames
#' @param y_var string name of column of observed values
#' @param group_vars string vector names of columns used to group means. Usually refers to month of year and orgUnit.
#' @returns data.frame of prediction intervals and observed values containing columns for group_vars and whether data is analysis or assessment
fit_naive <- function(cv_set, y_var, group_vars){

  #to solve global function binding
  # y_obs <- NULL
  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = group_vars, remove_NA = TRUE)
  this_analysis <- cv_clean$analysis
  this_assess <- cv_clean$assess

  this_preds <- this_analysis |>
    dplyr::summarise(quant_0.5 = mean(.data$y_obs),
                     quant_0.025 = min(.data$y_obs),
                     quant_0.975 = max(.data$y_obs),
                     .by = all_of(group_vars)) |>
    tidyr::pivot_longer(-all_of(group_vars), names_to = "quant_long", values_to = "predicted") |>
    dplyr::mutate(quantile_level = as.numeric(gsub("quant_","", .data$quant_long)))

  mod_pi <- dplyr::bind_rows(dplyr::mutate(this_analysis, dataset = "analysis"),
                             dplyr::mutate(this_assess, dataset = "assess")) |>
    dplyr::select(all_of(c(group_vars, "y_obs", "dataset", "date"))) |>
    dplyr::left_join(this_preds, by = group_vars, relationship = "many-to-many") |>
    dplyr::select(all_of(c("orgUnit", "date", "dataset", "observed" = "y_obs",
                  "predicted", "quant_long", "quantile_level")))
}
