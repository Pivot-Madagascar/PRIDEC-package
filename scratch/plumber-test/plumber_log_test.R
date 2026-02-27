# plumber_log_test.R for plumber app

# setwd("~/Dropbox/PIVOT/pride-c/packages/PRIDEC-package/scratch/plumber-test")
#testing with plumber2
# library(plumber2)
# test_api <- plumber2::api("plumber_log_test.R")
# plumber2::api_run(test_api, port = 8084)
# plumber2::api_stop(test_api)


# testing with plumber
# library(plumber)
# pr("plumber_log_test.R")|>
#     pr_run(port=8084)


#* Test returning a log with plumber2
#* @post /log_test
#* @serializer json
function() {

  create_message <- function(){

    message("this is a test")
    message("this is a test2")

    return(4)
  }
  #
  # file.create("test.log")
  # log_con <- file("test.log",
  #                 open = "a")


  error_log <- capture.output(create_message()) #doesn't work either

  # sink(type = "message")
  # close(log_con)

  # error_log <- readLines("test.log", warn = FALSE)
  # error_log <- PRIDEC::clean_ansi_log(error_log)
  # file.remove("test.log")

  return(list(message = "SUCCESS: All inputs valid.",
                status = 'success',
                code = 200,
                log = error_log))

}
