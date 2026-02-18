#' Format forecasts to be posted to DHIS2 instance
#' @param forecast_out data.frame output of `ensemble_forecast`
#' @param disease_dataElement dhis2 code that represents the dataElement to be forecast
#' @param forecast_start forecast start in period `YYYYMM` format
#' @param month_assess months of data to forecast forward. integer
#' @param month_analysis months of data to use to train the model. integer
#' @returns cleaned formatted data.frame for DHIS2
#' @export
format_forecast_dhis <- function(forecast_out, disease_dataElement, forecast_start,
                                 month_assess, month_analysis){

  start_date <- as.Date(paste0(forecast_start,"01"), format = "%Y%m%d")

  forecast_format <- forecast_out |>
    dplyr::filter(dataset == "assess") |>
    dplyr::filter(date < (start_date + months(month_assess)),
                  date > (start_date - months(month_analysis))) |>
    dplyr::mutate(predicted = as.integer(round(predicted)),
                  period = paste0(substr(date,1,4), substr(date,6,7))) |>
    dplyr::mutate(dataElement = dplyr::case_when(
      quantile_level < 0.5 ~ paste0(gsub("historic", "forecast", disease_dataElement), "LowCI"),
      quantile_level == 0.5  ~ paste0(gsub("historic", "forecast", disease_dataElement), "Avg"),
      quantile_level > 0.5~ paste0(gsub("historic", "forecast", disease_dataElement), "UppCI"),
    )) |>
    dplyr::mutate(categoryOptionCombo = "pridec_COC_u5") |>
    dplyr::select(orgUnit, period, value = predicted, dataElement, categoryOptionCombo) |>
    dplyr::distinct()

  validate_dhis_format(forecast_format, quiet = TRUE)

  return(forecast_format)


}


#' Validate formatted forecast data for DHIS2
#' @param this_df formatted DHIS2 forecast
#' @param quiet whether to print number of errors
#' @returns prints number of NA, non-integer, and negative values. These should all be zero
#' @export
validate_dhis_format <- function(this_df, quiet = FALSE){

  if(nrow(this_df)==0){
    stop("Forecast dataset is empty.", .call = FALSE)
  }

  this_value <- this_df$value
  err_1 <- sum(is.na(this_value))
  err_2 <- sum(!is.integer(this_value))
  err_3 <- sum(this_value<0, na.rm = TRUE)

  if(!quiet){
    print(paste("numNA:", err_1))
    print(paste("non-integers:", err_2))
    print(paste("negative values:", err_3))
  }


  if(sum(err_1,err_2,err_3)>0) {
    stop("Forecast contains non-real or negative numbers. Inspect `ensemble_forecast` outputs.",
         call. = FALSE)
  }

}
