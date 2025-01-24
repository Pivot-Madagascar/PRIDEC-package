#' Internal function to format cv_set for analysis
#' @param cv_set list of analysis and assessment data.frames
#' @param y_var name of variable to predict
#' @param pred_vars predictor variables used in model, or grouping variables in the case of [fit_naive()]. Default = NULL
#' @param remove_NA whether to remove missing y observations from analysis data. Default = FALSE
#' @returns list of `analysis` and `assess` dataframes
get_cv_subsets <- function(cv_set, y_var, pred_vars = NULL, remove_NA = FALSE){
  #to solve global function binding
  y_obs <- NULL

  this_analysis <- cv_set$analysis
  names(this_analysis)[names(this_analysis) == y_var] <- "y_obs"
  # this_analysis$y_obs <- this_analysis[[y_var]]
  this_analysis <- this_analysis[unique(c("y_obs", "orgUnit", "date", pred_vars))]
  this_analysis$dataset <- "analysis"

  if(remove_NA){
    this_analysis <- this_analysis[!is.na(this_analysis$y_obs),]
  }

  this_assess <<- cv_set$assessment
  names(this_assess)[names(this_assess) == y_var] <- "y_obs"
  this_assess <- this_assess[unique(c("y_obs", "orgUnit", "date", pred_vars))]
  this_assess$dataset <- "assess"

  return(list("analysis" = this_analysis,
              "assess" = this_assess))
}

#' Simulate a data.frame prediction interval to use in tests
#' @param n_orgUnit number of orgUnits
#' @param n_months number of months of analysis data, uses 3 months of assess
#' @param n_quant number of quantiles to use in prediction intervals
#' @returns data.frame of prediction intervals
create_demo_preds <- function(n_orgUnit, n_months, n_quant = 2){
  demo_data <- expand.grid(orgUnit = paste0("org_", LETTERS[1:n_orgUnit]),
                           date = seq.Date(as.Date("2020-01-01"), by = "month",
                                           length.out = n_months + 3))
  demo_data$observed <- stats::runif(nrow(demo_data),0, 100)
  demo_data$dataset <- ifelse(demo_data$date<=(as.Date("2020-01-01") + months(n_months)), "analysis", "assess")
  demo_data$quant_0.5 <- demo_data$observed + stats::rnorm(nrow(demo_data))*5
  demo_data$quant_0.95 <- demo_data$quant_0.5*1.4
  demo_data$quant_0.05 <- demo_data$quant_0.5*0.6
  demo_data <- tidyr::pivot_longer(demo_data, col = dplyr::starts_with("quant"),
                                   names_to = "quant_long", values_to = "predicted")
  demo_data$quantile_level <- as.numeric(gsub("quant_","", demo_data$quant_long))

  return(demo_data)
}
