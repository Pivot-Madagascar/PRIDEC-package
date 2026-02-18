#testing plumber functions pre-plumber

test_folder <- "/home/mevans/Dropbox/PIVOT/pride-c/appDev/pridec-pivot-update/input/"

config <- jsonlite::fromJSON(file.path(test_folder, "config.json"))
config$model_weights$weight[1] <- 0 #drop INLA for testing
external_data <- read.csv(file.path(test_folder, "external_data.csv"))
disease_data <- jsonlite::fromJSON(file.path(test_folder, "disease_data.json"))$dataValues
climate_data <- jsonlite::fromJSON(file.path(test_folder, "climate_data.json"))$dataValues
orgUnit_poly <- sf::st_read(file.path(test_folder, "orgUnit_poly.geojson"))

#1. Validate input data

PRIDEC::validate_inputs(config = config,
                        external_data = external_data,
                        disease_data = disease_data,
                        climate_data = climate_data,
                        orgUnit_poly = orgUnit_poly)
# 0 = success

#2. Import inputs and load forecast
input_list <- PRIDEC::validate_inputs(config = config,
                        external_data = external_data,
                        disease_data = disease_data,
                        climate_data = climate_data,
                        orgUnit_poly = orgUnit_poly,
                        return_inputs = TRUE)
names(input_list) == c("config", "input_data", "graph_poly")

output_dir <- normalizePath("scratch/forecast_output")

PRIDEC::run_pridec_forecast(inputs = input_list,
                    output_dir = output_dir)

cli::cli_h2("Creating HTML report of forecast")
PRIDEC::create_forecast_report(report_dir = output_dir, quiet = FALSE)


