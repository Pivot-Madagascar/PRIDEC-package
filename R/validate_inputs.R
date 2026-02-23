#' Validate input data
#'
#' Validates the input data for a PRIDE-C forecasting workflow
#'
#' @details
#' The `config` argument should have the following elements:
#' \itemize{
#'    \item pred_vars (string): vector of predictor variable names
#'    \item model_weights (data.frame): columns model and weight
#'    \item inla_hyper (list) : list of inla hyperparameters
#'    \item ranger_hyper (list) : list of ranger hyperparameters
#'    \item quantile_levels (numeric, 3)
#'    \item month_analysis (integer, 1)
#'    \item month_assess (integer, 1)
#'    \item month_lag (integer, 1). Defaults to 3
#'    \item forecast_start (string): date in format YYYYMM
#' }
#'
#' @param config list of configurations for forecase. See Details
#' @param external_data data.frame containing external data to use in prediction. Must contain columns orgUnit, period, and predictor variables specified in the config (optional)
#' @param disease_data data.frame containing historical disease data used to train model. Must contain columns orgUnit, period, dataElement, value
#' @param climate_data data.frame containing historical disease data used to train model. Must contain columns orgUnit, period, dataElement, value
#' @param orgUnit_poly sf data.frame containing geometry of orgUnits. Must contain column orgUnit
#' @param return_inputs whether to return validated input or not. If TRUE, returns list of configs, input data, and orgPoly
#' @returns T for success, F for error. Or if return_inputs=TRUE, list of validated inputs
#' @export
validate_inputs <- function(config,
                            external_data = NULL,
                            disease_data,
                            climate_data,
                            orgUnit_poly,
                            return_inputs = FALSE){
  #for testing
  # inspect = FALSE
  # if(inspect){
  #   test_folder <- "/home/mevans/Dropbox/PIVOT/pride-c/appDev/pridec-pivot-update/input/"
  #   config <- jsonlite::fromJSON(paste0(test_folder, "config.json"))
  #   external_data <- read.csv(paste0(test_folder, "external_data.csv"))
  #   disease_data <- jsonlite::fromJSON(paste0(test_folder, "disease_data.json"))$dataValues
  #   climate_data <- jsonlite::fromJSON(paste0(test_folder, "climate_data.json"))$dataValues
  #   orgUnit_poly <- sf::st_read(paste0(test_folder, "orgUnit_poly.geojson"))
  #   return_inputs = FALSE
  # }


  err_count <- 0


  #-------------Check configurations-------------
  missing_modelWeight <- c("inla", "glm_nb" ,"ranger", "arimax", "naive")[which(!(any((c("inla", "glm_nb" ,"ranger", "arimax", "naive") %in% config$model_weights$model))))]
  if(length(missing_modelWeight)>0){
    message(c("\nERROR in config: missing the following model weights: ",
                            paste(missing_modelWeight, collapse = ", "),
                            "\nSupply a weight of `0` for models you don't want to include. \n"))
    err_count <- err_count + 1
  }

  #set to default value if not provided
  if(is.null(config$inla_hyper)){
    message("\nNo inla_hyper provided. Setting to default values.")
    config$inla_hyper <- list("prec.unstruct" = c(1, 5e-4),
                               "prec.spatial" = c(1, 5e-4),
                               "prec.timerw1" = c(1,0.01))
  } else {
    if(!all(names(config$inla_hyper) == c("prec.unstruct", "prec.spatial", "prec.timerw1")) |
       !(all(sapply(config$inla_hyper, length) == 2)) |
       !(all(sapply(config$inla_hyper, class) == "numeric"))){
      message(c("\nERROR in config: `inla_hyper` poorly specified.
                `prec.unstruct`, `prec.spatial`, `prec.timerw1` should be numeric vectors of length 2."))

      err_count <- err_count + 1
    }
  }

  #set to default value if not provided
  if(is.null(config$ranger_hyper)){
    config$ranger_hyper <- list("mtry" = NULL,
                              "min.node.size" = NULL,
                              "num.trees" = 500)
    message("No ranger_hyper provided. Setting to default values.")
  } else {
    if(!all(names(config$ranger_hyper) == c("mtry", "min.node.size", "num.trees"))){
      message(c("\nERROR in config: `ranger_hyper` poorly specified.
                 Should include `mtry`, `min.node.size`, `num.trees`"))

      err_count <- err_count + 1
    }
  }

  if(length(config$quantile_levels) != 3){
    message("\nERROR in config: Quantile levels must be numeric of length 3.")
    err_count <- err_count + 1
  }

  if(!is.integer(config$month_analysis)){
    message("\nERROR in config: month_analysis must be an integer of length 1")
    err_count <- err_count + 1
  }

  if(!is.integer(config$month_assess)){
    message("\nERROR in config: month_assess must be an integer of length 1")
    err_count <- err_count + 1
  }


  if(is.null(config$month_lag)){
    config$month_lag <- 3
    message("No month_lag provided. Setting to default of 3.")
  } else {
    config$month_lag <- as.numeric(config$month_lag)
  }

  #defaut is current month
  if(is.null(config$forecast_start)){
    message("No forecast_start provided. Setting to current month.")
    config$forecast_start <- gsub("-","",substr(lubridate::rollback(Sys.Date(), roll_to_first = TRUE), 1,7))
  }

  if(is.na(as.Date(paste0(config$forecast_start, "01"), format = "%Y%m%d"))){
    message("\nERROR in config: forecast_start not a valid date. Is it in YYYYMM format?")
    err_count <- err_count + 1
  }

  #add disease element to configs
  config$disease_dataElement <- unique(disease_data$dataElement)


  # Ensure appropriate columns in all data ----------------
  if(!is.null(external_data)){
    if(check_columns(external_data,  c("orgUnit","period"))>0){
      message(paste("\nERROR: Columns missing from external_data: ",
                              paste(check_columns(external_data,  c("orgUnit","period")),
                                    collapse = ", ")))
      err_count <- err_count + 1
    }
  }

  if(check_columns(disease_data,  c("orgUnit","period", "dataElement", "value"))>0){
    message(paste("\nERROR: Columns missing from disease_data: ",
                                paste(check_columns(disease_data,  c("orgUnit","period", "dataElement", "value")),
                                      collapse = ", ")))
    err_count <- err_count + 1
  }

  if(check_columns(climate_data,  c("orgUnit","period", "dataElement", "value"))>0){
    message(paste("\nERROR: Columns missing from climate_data: ",
                                paste(check_columns(climate_data,  c("orgUnit","period", "dataElement", "value")),
                                      collapse = ", ")))
    err_count <- err_count + 1
  }

  if(check_columns(orgUnit_poly,  c("orgUnit"))>0){
    message(paste("\nERROR: Columns missing from orgUnit_poly: ",
                                paste(check_columns(orgUnit_poly,  c("orgUnit")),
                                      collapse = ", ")))
    err_count <- err_count + 1
  }

  # ensure period is a character, not numeric
  external_data$period <- as.character(external_data$period)
  disease_data$period <- as.character(disease_data$period)
  climate_data$period <- as.character(climate_data$period)

  #------------External data -------------
  if(!is.null(external_data)){
    #check for NAs
    if(sum(is.na(external_data))>0){
      na_col <- colSums(is.na(external_data))
      na_col <- na_col[na_col>0]
      message(paste("\nERROR: External data has `NA` in the following columns: ",
                                  paste(names(na_col),
                                        collapse = ", ")))
      err_count <- err_count + 1
    }

  }

  #--------------Disease Data -------------
  disease_data <- disease_data[,c("orgUnit", "period", "dataElement", "value")]

  if(length(unique(disease_data$dataElement))>1){
    message(paste("\nERROR: `disease_data` should contain one dataElement but contains multiple:\n",
                                paste(unique(disease_data$dataElement),
                                      collapse = ", ")))
    err_count <- err_count + 1
  }

  if(all(sapply(disease_data, class) != c(rep("character",3), "numeric"))){
    wrong_class <- colnames(disease_data)[sapply(disease_data, class) != c(rep("character",3), "numeric")]
    message(paste("\nERROR: The following `disease_data` columns are incorrectly specified:",
                                paste(wrong_class,
                                      collapse = ", "),
                                "\n Correct classes are orgUnit (chr), period (chr), dataElement (chr), value (num)"))
  err_count <- err_count + 1
    }

  # ------------Combine into Input data ------------

  #check that all predictor variables are there
  missing_predVars <- config$pred_vars[which(!(config$pred_vars %in% c(unique(climate_data$dataElement), colnames(external_data))))]
  if(length(missing_predVars)>0){
    message(c("\nERROR: The following predictor variables are missing from the input datasets:\n",
              paste(missing_predVars, collapse = ", "),
              "\n \n Ensure they are present in `climate_data` or `external_data`."))

    input_data <- NULL
    err_count <- err_count + 1
  } else {

    input_data <- dplyr::bind_rows(disease_data, climate_data) |>
      tidyr::pivot_wider(names_from = "dataElement", values_from = "value") |>
      dplyr::full_join(external_data, by = c("orgUnit", "period")) |>
      #the joins above will sometimes add variables for orgUnits we don't have data for, drop them
      dplyr::filter(orgUnit %in% disease_data$orgUnit)

    #limit to predictor variables only to save space
    input_data <- input_data[,c("orgUnit", "period", config$disease_dataElement, config$pred_vars)]
  }

  #---------------geojson polygons---------------
  orgUnit_poly$org_ID <- 1:nrow(orgUnit_poly)

  #does every org unit have an associated polygon?
  all_ou <- unique(input_data$orgUnit)
  missing_orgPoly <-all_ou[which(!(all_ou %in% orgUnit_poly$orgUnit))]
  if(length(missing_orgPoly)>0){
    message(c("\nERROR: The following orgUnits are missing corresponding polygons in `orgUnit_poly`:\n",
                            paste(missing_orgPoly, "\n")))
    err_count <- err_count + 1
  }

  # --------------- Return data or errors -----------------#
  if(err_count>0){
    message("\nERROR: Invalid inputs. See notes above.")
    return(FALSE)
  } else {
    message("\nSUCCESS: All inputs valid.")

    if(return_inputs){

      message("Returning processed inputs as a list.")
      return(list(config = config,
                  input_data = input_data,
                  graph_poly = orgUnit_poly))
    } else {
      return(TRUE)
    }

  }
}

#' Check that dataframe contains appropriate columns
#' @param input_data data.frame to check
#' @param column_names required column names
#' @returns names of missing columns (if missing), or 0 if none missing
check_columns <- function(input_data, column_names){
  found <- column_names %in% colnames(input_data)
  if(!all(found)){
    return(column_names[!found])
  } else {
    return(FALSE)
  }
}
