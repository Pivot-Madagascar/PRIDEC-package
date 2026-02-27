#' Create HTML forecast report from quarto template
#' @param report_dir directory to build quarto doc in
#' @param quiet whether to render quarto doc quietly
#' @returns TRUE if successful, FALSE if error
#' @export
#'
create_forecast_report <- function(report_dir,
                                   quiet = TRUE){

  tryCatch({
    #move files over and save
    #move template to output to run
    template_file <- system.file("quarto_templates/forecastValidation-template_en.qmd",
                                 package = "PRIDEC")
    file.copy(template_file, to = file.path(report_dir, "tmp_template.qmd"),
              overwrite = TRUE)


    cmd <- paste0("cd ", normalizePath(report_dir), " && quarto render tmp_template.qmd --output forecast_report.html -M title:'PRIDE-C Forecast Report'")

    if (quiet) {
      cmd <- paste(cmd, "--quiet")
    }
    result <- system(cmd)

    file.remove(file.path(report_dir, "tmp_template.qmd"))

    if (result == 0) {
      message(paste("Created forecast report at:", file.path(gsub("\\/", "", report_dir), "forecast_report.html")))
      return(TRUE)
    } else {
      message(paste("Quarto render failed with exit code:", result))
    }

  }, error = function(e) {
    message(paste("Error in create_forecast_report:", e$message))
    # Create a simple fallback HTML report
    message("Creating simple report...")
    create_simple_report(report_dir)

    return(FALSE)
  })
}

#' Simple report to provide when there is a quarto error
#' @param report_dir directory to save report in
#' @returns Nothing, but creates html document in `report_dir`
create_simple_report <- function(report_dir) {
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>Simple PRIDE-C Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        h1 { color: #333; }
        p { line-height: 1.6; }
    </style>
</head>
<body>
    <p>This is a simplified forecast report generated because the full Quarto report could not be created.</p>
    <p>Please check the forecast data in the output directory.</p>
    <p>Report generated on: %s</p>
</body>
</html>
', Sys.time())

  writeLines(html_content, file.path(report_dir, "forecast_report.html"))

}
