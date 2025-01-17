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
  this_analysis$y_obs <- this_analysis$true_value <- this_analysis[[y_var]]
  this_analysis <- this_analysis[unique(c("y_obs", "true_value", "orgUnit", "date", pred_vars))]

  if(remove_NA){
    this_analysis <- this_analysis[!is.na(this_analysis$y_obs),]
  }

  this_assess <<- cv_set$assessment
  this_assess$y_obs <- this_assess$true_value <- this_assess[[y_var]]
  this_assess <- this_assess[unique(c("y_obs", "true_value", "orgUnit", "date", pred_vars))]

  return(list("analysis" = this_analysis,
              "assess" = this_assess))
}
