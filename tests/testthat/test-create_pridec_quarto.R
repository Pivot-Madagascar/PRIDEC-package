test_that("Quarto results creation works", {
  skip("requires model results that are not on CRAN")

  create_pridec_quarto(results_dir = "/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/demo_trainModelResults",
                       html_filename = "/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/demo-test.html",
                       lang = "fr",
                       doc_title = "Demo Quarto Report")

  expect_true(file.exists("/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/demo-test.html"))

  browseURL("file://home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/demo-test.html")

  #clean up after the test
  file.remove("/home/mevans/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/demo-test.html")
})
