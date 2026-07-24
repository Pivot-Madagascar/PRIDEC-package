#' Function used to download disease data from DHIS2
#' @param dx_query query to use for dimension. in format of dx:uid for disease
#' @param ou_query query used to get the needed orgUnit resolution.
#' Default is all Formation Sanitaires in Ifanadiana. Format is ou:query
#' @param co_query query used for category option combos, such as age or sex
#' @param pe_query vector of months for periods you want to download. Format  is pe:YYYYMM;YYYYMM
#' @param includeNumDen whether to download the numerator and denominator of indicators
#' @param base_url_req base API request in httr2 object form to use, including authorization
#' @param pause amount of seconds to pause after an API call. Used when applying over multiple queries. Default = 0
get_dhis_data <- function(dx_query,
                     ou_query = "ou:LEVEL-z7UyvgYEMRa;VtP4BdCeXIo",
                     co_query = NULL,
                     pe_query,
                     includeNumDen = FALSE,
                     base_url_req,
                     pause = 0){

  GETrequest <-  base_url_req |>
    httr2::req_url_query(dimension = dx_query,
                  dimension = ou_query,
                  dimension = pe_query,
                  dimension = co_query,
                  includeNumDen = includeNumDen)


  dataReq <- httr2::req_perform(GETrequest)
  dataResponse <- dataReq |>
    httr2::resp_body_string() |>
    readr::read_csv(show_col_types = FALSE) |>
    as.data.frame()

  Sys.sleep(pause)

  return(dataResponse)
}
