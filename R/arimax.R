#' fit an ARIMAX model to one orgUnit
#' @param train_df dataframe of analysis data
#' @param test_df dataframe of assessment data
#' @param pred_vars predictor variables to include
#' @param quant_levels quantiles needed for prediction intervals (assessment only)
#' @param return_model whether or not to return the ARIMA model, mostly used for debugging. Default = FALSE
#' @param log_trans whether to log transform the data to help with fit. Uses log(y+1). Default = FALSE
#' @returns dataframe of prediction intervals for analysis and assessment data
fit_arima_OneOrgUnit <- function(train_df, test_df, pred_vars,
                                 quant_levels, return_model = FALSE,
                                 log_trans = FALSE){

  #if it starts with an NA, will not return fitted for that value
  #cut data until it starts with a true value
  first_val <- is.na(train_df$y_obs[1])
  while(first_val){
    train_df <- train_df[-1,]
    first_val <- is.na(train_df$y_obs[1])
  }

  if(log_trans){
    train_df$y_obs <- log(train_df$y_obs +1)
    test_df$y_obs <- log(test_df$y_obs+1)
  }

  this_y_ts <- stats::ts(data = train_df$y_obs,
                  start = c(lubridate::year(min(train_df$date)),
                            lubridate::month(min(train_df$date[1]))),
                  frequency = 12)

  #check that pred_vars are not static
  #any static variables are removed from the pred_vars list
  n_unique_pred <- sapply(lapply(train_df[pred_vars], FUN = unique, MARGIN = 2), length)
  if(any(n_unique_pred<2)){
    warning(paste("Static variable found in ARIMA model:", pred_vars[n_unique_pred<2], "\nFitting without static variables."))
    pred_vars <- pred_vars[n_unique_pred>1]
  }

  arimax_mod <- retry_arima(train_df = train_df, pred_vars = pred_vars, this_y_ts = this_y_ts)

  #get preds for historical (can only get median fit, not PIs)
  arimax_pi_analysis <- data.frame(predicted = as.numeric(arimax_mod$fitted),
                                   date = train_df$date,
                                   observed = train_df$y_obs,
                                   quantile_level = 0.5,
                                   quant_long = "quant_0.5",
                                   orgUnit = unique(train_df$orgUnit),
                                   dataset = "analysis")

  arimax_pi_assess <- get_arima_pi(arima_mod = arimax_mod, quant_levels = quant_levels,
                                   xreg = as.matrix(test_df[pred_vars])) |>
    dplyr::mutate(orgUnit = unique(test_df$orgUnit),
           dataset = "assess") |>
    dplyr::left_join(dplyr::select(test_df, all_of(c("observed" = "y_obs", "orgUnit", "date"))),
                     by = c("date", "orgUnit"))

  both_pi <- dplyr::bind_rows(arimax_pi_analysis, arimax_pi_assess)

  if(log_trans){
    both_pi$predicted <- exp(both_pi$predicted)-1
    both_pi$observed <- exp(both_pi$observed)-1
  }
    #truncate at zero?
  both_pi <- both_pi |>
    dplyr::mutate(predicted = ifelse(.data$predicted<0,0, .data$predicted))
  if(return_model){
    return(list("model" = arimax_mod,
                "predictions" = both_pi))
  } else return(both_pi)
}

#' Fit an ARIMAX model to a cv_set
#'
#' Fucntion to apply the ARIMAX workflow to each orgUnit in a dataset
#' @param cv_set list containing `analysis` and `assessment` data.frames. One
#'   element of the output of [split_cv_rolling()].
#' @param y_var character string of y variable label
#' @param pred_vars vector string of column names of predictor variables. Note
#'   that static variables cannot be included in an ARIMAX model and will
#'   be automatically removed.
#' @param quant_levels numeric vector of quantiles to use in prediction
#'   intervals. Range 0-1. Default: c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25,
#'   0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
#'   0.95, 0.975, 0.99)
#' @param log_trans whether to log transform the data to help with fit. Uses log(y+1). Default = FALSE
#' @returns data.frame of prediction intervals on analysis and assessment data

fit_arima <- function(cv_set, y_var, pred_vars,
                         quant_levels = c(0.01,0.025, seq(0.05,0.95, by = 0.05), 0.975, 0.99),
                      log_trans = FALSE){

  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = pred_vars)
  this_analysis <- cv_clean$analysis
  this_assess <- cv_clean$assess

  preds_pi <- purrr::map(unique(this_analysis$orgUnit),
                  function(x) fit_arima_OneOrgUnit(train_df = this_analysis[(this_analysis$orgUnit == x),],
                                                   test_df = this_assess[(this_assess$orgUnit == x),],
                                                   pred_vars = pred_vars,
                                                   quant_levels = quant_levels,
                                                   log_trans = log_trans)) |>
    dplyr::bind_rows()

  return(preds_pi)

}



#' Internal function to iteratively fit an ARIMAX model
#'
#' This function fits an ARIMAX model iteratively. It starts with all of the e
#'   exogeneous variables and if an error is thrown, it removes the last one in
#'   the list and tries to refit. It repeats this process until a model is
#'   successfully fit. If no models fit, an ARIMA model (without exogenous
#'   variables) is returned. The model is fit via forecast::auto.arima()
#' @param train_df data.frame of data used to train the model
#' @param pred_vars vector of character names of exogeneous variables
#' @param this_y_ts time series object of observed y variables from train_df. Created via stats::ts().
#' @return a fit ARIMAX or ARIMA model, fit via. auto.arima.
retry_arima <- function(train_df, pred_vars, this_y_ts){
  new_pred_vars <- pred_vars

  #model without exogeneous variables
  arima_mod <- forecast::auto.arima(this_y_ts) #to return in case it fails

  while(length(new_pred_vars)>0){
    try({
      this_x_ts <- stats::ts(data = train_df[pred_vars],
                      start = c(lubridate::year(train_df$date[1]),
                                lubridate::month(train_df$date[1])),
                      frequency = 12)

      arimax_mod <- forecast::auto.arima(this_y_ts,
                               xreg = this_x_ts)
      return(arimax_mod)
    }, silent = TRUE)
    cli::cli_alert_warning(paste("Error fitting ARIMAX. Dropping", new_pred_vars[length(new_pred_vars)],
                                  "and refitting."))
    new_pred_vars <- new_pred_vars[-length(new_pred_vars)]
  }

  #If still fails
  cli::cli_alert_warning("ARIMAX cannot fit with exogenous variables. Fitting ARIMA without xreg.")
  return(arima_mod)
}

#' Internal function for estimating prediction intervals from ARIMAX model
#' @param arima_mod ARIMAX model object
#' @param quant_levels quantile levels to estimate for the prediction intervals. Numeric 0-1
#' @param h Arima forecast horizon, in months. Default = 3
#' @param xreg corresponds to exogeneous variables in ARIMAX model (optional)
#' @returns data.frame of prediction intervals with columns date, prediction,
#'          quantile, and quant_long. Only for assessment data.
get_arima_pi <- function(arima_mod, quant_levels,
                         h = 3,
                         xreg = NULL){

  arima_levels <- unique(round(sort(abs(quant_levels-0.5)*2),2))*100

  arima_pred <- generics::forecast(arima_mod, h=h, level = arima_levels, xreg = xreg)
  arima_pis <- suppressWarnings(data.frame(arima_pred))

  colnames(arima_pis) <- sub("\\.", "_", colnames(arima_pis), fixed = FALSE)

  pred_pi <- arima_pis |>
    tibble::rownames_to_column(var = "month_label") |>
    tidyr::pivot_longer(-all_of("month_label")) |>
    tidyr::separate(.data$name, into = c("bound", "pi_value"), sep = "_") |>
    dplyr::filter(!(.data$bound == "Point")) |>
    dplyr::mutate(pi_value = as.numeric(.data$pi_value)/100/2) |>
    dplyr::mutate(quantile_level = dplyr::case_when(
      .data$bound == "Point" ~ 0.5,
      .data$bound == "Lo" ~ 0.5 - .data$pi_value,
      .data$bound == "Hi" ~ 0.5 + .data$pi_value
    )) |>
    dplyr::mutate(quant_long = paste0("quant_", as.character(.data$quantile_level))) |>
    dplyr::mutate(date = as.Date(paste("01", .data$month_label), format = "%d %B %Y")) |>
    dplyr::select(all_of(c("date", "predicted" = "value", "quantile_level", "quant_long"))) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$date, .data$quant_long)

  return(pred_pi)
}
