#code to create `forecast-inputs.Rds`

#' These are the files used in the automated PRIDE-C forecast with DHIS2, and we need
#' them to test some of our wrapper functions.
#'
#'


config <- jsonlite::fromJSON("data-raw/forecast-inputs/config.json")
external_data <- read.csv("data-raw/forecast-inputs/external_data.csv")
disease_data <- jsonlite::fromJSON("data-raw/forecast-inputs/disease_data.json")$dataValues
climate_data <- jsonlite::fromJSON("data-raw/forecast-inputs/climate_data.json")$dataValues
orgUnit_poly <- sf::st_read("data-raw/forecast-inputs/orgUnit_poly.geojson")

#save image to load in test
save.image(file = "tests/testthat/fixtures/demo_inputs.Rds")
