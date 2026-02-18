#' Function to create quarto doc of model training from model outputs
#' @param results_dir path to directory where model outputs are saved
#' @param html_filename where you want the html file to be saved, including path
#' @param language which language template to use. currently only  "fr" available
#' @param doc_title title for HTML report. Default: "PRIDEC Rapport de Performance"
create_train_quarto <- function(results_dir,
                                html_filename,
                                lang = "fr",
                                doc_title = NULL){

  #for debug
  # results_dir <- "/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/demo_trainModelResults"
  # html_filename <- "/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/quarto-out-test.html"
  # lang = "fr"
  # doc_title = NULL

  if(is.null(doc_title)){
    doc_title <- "PRIDEC Rapport de Performance"
  }

  template_file <- system.file(paste0("quarto_templates/modelPerformance-template_", lang,".qmd"),
                               package = "PRIDEC")

  file.copy(template_file, to = "tmp_template.qmd",
            overwrite = TRUE)

  quarto::quarto_render(
    input = "tmp_template.qmd",
    output_file = "tmp_quarto-out.html",
    execute_params = list(results_dir = results_dir),
    quarto_args = c("--metadata", paste0("title=", doc_title))
  )
  file.copy("tmp_quarto-out.html", to = html_filename,
            overwrite = TRUE)
  file.remove("tmp_template.qmd")
  file.remove("tmp_quarto-out.html")

}
