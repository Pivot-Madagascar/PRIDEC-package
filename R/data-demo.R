#' Demo malaria datasets
#'
#' These datasets are simulated data representing monthly malaria cases and associated
#' covariates in the District of Ifanadiana, Vatovavy, Madagascar. They represent
#' 21 orgUnits and 106 months of data (October 2016 - July 2024). They are used in demos
#' and in internal testing.
#'
#' @format ## `demo_malaria`
#' A data frame with 2226 rows and 12 columns:
#' \describe{
#'  \item{orgUnit}{DHIS2 orgUnit corresponding to primary health clinics}
#'  \item{n_case}{Number of monthly malaria cases}
#'  \item{pev}{Average monthly potential evapotranspiration}
#'  \item{rain_mm}{Total monthly rainfall}
#'  \item{temp_c}{Average monthly temperature}
#'  \item{wealth_index}{Socio-economic index}
#'  \item{elevation}{Elevation}
#'  \item{LLIN_use}{Proportion of households with at least two LLINs per member}
#'  \item{csb_type}{Type of clinic: CSB1 or CSB2}
#'  \item{time_to_district}{Travel time to district capital from clinic}
#'  \item{LLIN_wane}{Number of years since LLIN distribution campaign}
#'  \item{period}{Year and month followin DHIS2 period style (YYYYMM)}
#' }
#' @source PRIDE-C research team
#'
