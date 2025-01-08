# Functions for preparing case data and variables for modeling

#' Prepare case data and predictor variables for forecast models
#' @param raw_data data.frame containing the variables
#' @param y_var name of variable to predict
#' @param lagged_vars string vector of names of variables to lag
#' @param scaled_vars string vector of names of variables to scale
#' @param lag_n number of months by which to lag the lagged variables. Default = 3.
#' @param graph_poly sf object to be used to create graph for INLA model
#'   (optional). Default = NULL.
#' @returns list of cleaned data, spatial graph for INLA (if not null), and
#'   scale and centering factors used in treatment
#' @export
prep_caseData <- function(raw_data,
                          y_var,
                          lagged_vars,
                          scaled_vars,
                          lag_n = 3,
                          graph_poly = NULL){

  raw_data$date <- as.Date(paste0(raw_data$period, "01"), format = "%Y%m%d")
  #update scaled variables to also include lagged variables
  scaled_vars <- c(scaled_vars, paste0(lagged_vars, "_lag"))

  data_prep <- raw_data |>
    dplyr::group_by(orgUnit) |>
    dplyr::arrange(date) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(lagged_vars),
                                ~ dplyr::lag(.x, lag_n),
                                .names = "{.col}_lag")) |>
    dplyr::ungroup() |>
    #rescale variables
    dplyr::mutate(dplyr::across((dplyr::starts_with(scaled_vars)), ~as.numeric(scale(.x, center = TRUE, scale = TRUE)),
                  .names = "{.col}sc")) |>
    #create month of year(month_season) and overall month (essentially date)
    dplyr::mutate(month_season = as.factor(lubridate::month(date)),
           month_num  = (lubridate::interval(min(date), date) %/% months(1))) |>
    dplyr::mutate(orgUnit = as.factor(orgUnit))


  #fx to extract what we used to scale and center in case we need to back-transform later
  extract_scale <- function(var_name){
    this_scale <- scale(data_prep[var_name], center = TRUE, scale = TRUE)

    data.frame(scale = attr(this_scale, "scaled:scale"),
               center = attr(this_scale, "scaled:center")) |>
      tibble::rownames_to_column(var = "variable") |>
      dplyr::mutate(variable = paste0(variable, "sc"))
  }

  scale_factors <- do.call(rbind,(lapply(scaled_vars, extract_scale)))

  data_prep <- fill_seasonal(data_prep,
                             data_col = paste0(lagged_vars, "_lagsc"),
                             group_col = "orgUnit")

  #create spatial graph for INLA
  if(!is.null(graph_poly)){
    W_orgUnit <- spdep::poly2nb(graph_poly, queen = TRUE) |>
      spdep::nb2mat(, style = "W")

    graph_ID <- graph_poly[,c("orgUnit", "org_ID")]
    graph_ID <- sf::st_set_geometry(graph_ID, NULL)
    data_prep <- data_prep |>
      dplyr::left_join(graph_ID, by = "orgUnit")


  } else {
    W_orgUnit <- NA
  }
  return(list("W_graph" = W_orgUnit,
              "data_prep" = data_prep,
              "scale_factors" = scale_factors))
}

#' Fill in missing data via seasonally decomposed imputation
#'
#' @param data_to_fix dataframe of columns to impute, must contain date
#' @param data_col names of vector columns of data to fill
#' @param group_col name of column representing orgUnit grouping
#' @param verbose whether to print warnings. Default = FALSE
#' @returns original data_to_fix dataframe with missing data imputed
fill_seasonal <- function(data_to_fix, data_col, group_col, verbose = FALSE){


  data_to_fix <- data_to_fix[order(data_to_fix[[group_col]], data_to_fix[["date"]]), ]

  for(this_col in data_col){
    #apply seasonal imputation over each orgUnit individually
    this_vec <- split(data_to_fix[this_col], f = data_to_fix[group_col])
    this_vec <- lapply(this_vec, FUN = function(x) stats::ts(x, deltat = 1/12))
    if(!verbose){
      suppressWarnings({
    data_to_fix[this_col] <- lapply(this_vec, FUN = imputeTS::na_seadec, find_frequency = TRUE,
                                    algorithm = "ma") |>
      unlist() |> c()
      })
    } else {
      data_to_fix[this_col] <- lapply(this_vec, FUN = imputeTS::na_seadec, find_frequency = TRUE,
                                      algorithm = "ma") |>
        unlist() |> c()
    }

  }
  return(data_to_fix)
}
