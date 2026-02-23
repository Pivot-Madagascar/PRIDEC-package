# plumber.R

# plumber2::api_run(plumber2::api("plumber.R"))

#load inputs into session memory
# not scalable, see https://www.rplumber.io/articles/execution-model.html#managing-state
#best is if these are accessible via REDIS
input_folder <- normalizePath("app/input")
config <- jsonlite::fromJSON(file.path(input_folder, "config.json"))
external_data <- read.csv(file.path(input_folder, "external_data.csv"))
climate_data <- jsonlite::fromJSON(file.path(input_folder, "disease_data.json"))$dataValues
climate_data <- jsonlite::fromJSON(file.path(input_folder, "climate_data.json"))$dataValues
orgUnit_poly <- sf::st_read(file.path(input_folder, "orgUnit_poly.geojson"), quiet = TRUE)

#these are updated after being successfully run to add a check
valid_inputs <- FALSE
forecast_status <- FALSE

# function(req, res){
#
#
#   input <- list()
#   req$session$input$config <- jsonlite::fromJSON(file.path(input_folder, "config.json"))
#   req$session$input$external_data <- read.csv(file.path(input_folder, "external_data.csv"))
#   req$session$input$disease_data <- jsonlite::fromJSON(file.path(input_folder, "disease_data.json"))$dataValues
#   req$session$input$climate_data <- jsonlite::fromJSON(file.path(input_folder, "climate_data.json"))$dataValues
#   req$session$input$orgUnit_poly <- sf::st_read(file.path(input_folder, "orgUnit_poly.geojson"), quiet = TRUE)
#
#   res$setHeader("Content-Type", "application/json")
#   return(jsonlite::toJSON(list(status = 'success',
#                      message = "Inputs successfully loaded into session memory.",
#                      code = 201)))
# }

#* Validate inputs in input/ folder
#* @post /validate_inputs
#* @serializer json
function() {

  file.create("pridec.log")
  log_con <- file("pridec.log",
                  open = "a")
  sink(log_con, append = TRUE, type = "message")

  validate_res <- PRIDEC::validate_inputs(config = config,
                          external_data = external_data,
                          disease_data = disease_data,
                          climate_data = climate_data,
                          orgUnit_poly = orgUnit_poly)
  sink(type = "message")
  close(log_con)

  error_log <- readLines("pridec.log", warn = FALSE)
  file.remove("pridec.log")

  if(validate_res){

    valid_inputs <<- TRUE
    return(list(message = "SUCCESS: All inputs valid.",
                       status = 'success',
                       code = 200,
                       log = error_log))
  } else {

    valid_inputs <<- FALSE

    return(list(message = "ERROR: Invalid inputs. See log for details",
                       status= 'error',
                       code = 400,
                       log = error_log))
  }
}

#* Run Forecast
#* @post /run_forecast
#* @serializer json
function() {

  if(!valid_inputs){
    return(list(status = "error",
                       message = "Inputs not validated. Run `POST /validate_inputs` first.",
                       code = 404))
  }

  file.create("pridec.log")
  log_con <- file("pridec.log",
                  open = "a")
  sink(log_con, append = TRUE, type = "message")

  input_list <- PRIDEC::validate_inputs(config = config,
                                          external_data = external_data,
                                          disease_data = disease_data,
                                          climate_data = climate_data,
                                          orgUnit_poly = orgUnit_poly,
                                        return_inputs = TRUE)

  output_dir <- normalizePath("app/output")

  forecast_status <<- PRIDEC::run_pridec_forecast(inputs = input_list,
                              output_dir = output_dir)

  if(forecast_status){
    message("SUCCESS: Forecast created.")
    message("Creating HTML report of forecast...")
    report_status <- PRIDEC::create_forecast_report(report_dir = output_dir, quiet = TRUE)

    sink(type = "message")
    close(log_con)

    error_log <- readLines("pridec.log", warn = FALSE)
    file.remove("pridec.log")

    if(report_status){
      return(list(message = "SUCCESS: Forecast and report created.",
                         status= 'success',
                         code = 200,
                         log = error_log))
    } else {
      return(list(message = "WARNING: Forecast created but report failed. Created simple report",
                         status= 'warning',
                         code = 200,
                         log = error_log))
    }

  } else {

    sink(type = "message")
    close(log_con)

    error_log <- readLines("pridec.log", warn = FALSE)
    file.remove("pridec.log")

    return(list(message = "ERROR: Forecast encountered error. See log for details.",
                       status= 'error',
                       code = 400,
                       log = error_log))

  }

}
