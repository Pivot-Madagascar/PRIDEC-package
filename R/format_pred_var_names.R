#' Format predictor variables names to account for lagging and scaling
#' @param var_list string vector of predictor variables
#' @param input_data data.frame of input data
#' @returns list of variable names, variables to lag, and variables to scale
#' @export
format_pred_vars <- function(var_list, input_data){
  dynamic_vars <- c("pridec_climate_precipitation", "pridec_climate_temperatureMean",
                    "pridec_climate_relHumidity", "pridec_climate_evi", "pridec_climate_mndwi",
                    "pridec_climate_gao", "pridec_climate_propFire", "pridec_climate_AOD",
                    "pridec_climate_windspeed", "pridec_climate_floodedRice")

  #lag the climate variables
  vars_to_lag <- var_list[var_list %in% dynamic_vars]
  #scale the numeric variables
  vars_to_scale <- colnames(Filter(is.numeric, input_data[,var_list]))

  #update names to match predictor variables after prep
  pred_vars <- var_list
  pred_vars[which(var_list %in% vars_to_lag)] <- paste0(pred_vars[which(var_list %in% vars_to_lag)], "_lag")
  pred_vars[which(var_list %in% vars_to_scale)] <- paste0(pred_vars[which(var_list %in% vars_to_scale)], "sc")

  return(list(pred_vars = pred_vars,
              vars_to_lag = vars_to_lag,
              vars_to_scale = vars_to_scale))
}
