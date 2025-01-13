#' Create cv folds within a dataset using rolling origin method
#' @param data_to_split data frame that you want to create folds from, must include date column
#' @param start_date date from which the folds will start, in Date class. default NULL = first date of dataset
#' @param month_analysis number of months to include in analysis sets. Default = 48
#' @param month_assess number of months to include in the assessment sets. Default = 3
#' @returns list of datasets including analysis and assessment set nested in each list object
#'
split_cv_rolling <- function(data_to_split, start_date = NULL,
                             month_analysis = 48, month_assess =3){

  #check that enough data is available to make at least one cv split
  if(!(length(unique(data_to_split$date))>=(month_analysis+month_assess))){
    avail_dates <- length(unique(data_to_split$date))
    cli::cli_abort(c("The data must contain enough dates to include your defined
                     periods for analysis and assessment.",
                     "i" = "The dataset contains {avail_dates} months of data, but you specified
                     {month_analysis} months of analysis and {month_assess} of assessment. Reduce
                     {.var month_analysis} or {.var month_assess} to fit your data."))
  }

  if(is.null(start_date)){
    start_date <- min(data_to_split$date)
  }

  #count number of folds possible
  num_folds <- lubridate::interval(start_date + months(month_analysis) + months(month_assess),
                                   max(data_to_split$date)) %/% months(1)
  cv_list <- list()
  for(i in 1:num_folds){
    analysis_inds <- dplyr::between(data_to_split$date, start_date + months(i - 1), start_date + months(i - 2) + months(month_analysis))
    assess_inds <- dplyr::between(data_to_split$date, start_date + months(i - 2) + months(month_analysis) + months(1), start_date + months(i - 2) + months(month_analysis) + months(month_assess))

    cv_list[[i]] <- list(analysis = data_to_split[analysis_inds,],
                         assessment = data_to_split[assess_inds,])
  }
  return(cv_list)

}

#' Split the dataset into a training and testing set
#'
#' @details
#' Dataset is stratified by year, with an assessment period beginning in four months
#' of every year
#'
#' @param cvdata_list list of cv folds (output of split_cv_rolling)
#' @returns list of cv folds to be used in training ("train_data") and testing ("test_data")
split_stratified <- function(cvdata_list){

  # 30% as test data (4 of every 12)
  num_yrs <- floor(length(cvdata_list) /12)
  test_inds <- c()
  for(i in 0:(num_yrs-1)){
    this_sample <- sample(1:12,4) + i*12
    test_inds <- c(test_inds, this_sample)
  }
  last_sample <- sample(((num_yrs)*12+1):length(cvdata_list), floor((length(cvdata_list)-((num_yrs)*12))/3))
  test_inds <- c(test_inds, last_sample)
  test_inds <- unique(sort(test_inds))

  train_data <- cvdata_list[-test_inds]
  test_data <- cvdata_list[test_inds]

  return(list("train_data" = train_data,
              "test_data" = test_data))
}
