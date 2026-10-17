test_that("get dhis data works", {
  skip("WIP")
  period_query <- paste0("pe:", paste(c("202501", "202502"), collapse = ";"))
  dataElement <- "dx:Uvn6LCg7dVU"
  ou_query <- c("dGheVylzol6", "zFDYIgyGmXG")
  ou_query <- paste0("ou:", paste(ou_query, collapse = ";"))

  demo_url <- "https://play.im.dhis2.org/stable-2-42-4/api/analytics.csv?"
  demo_req <- httr2::request(demo_url) |>
    httr2::req_auth_basic(username = "admin",
                   password = "district")

  get_dhis_data(dx_query = dataElement,
                ou_query = ou_query,
                pe_query = period_query,
                base_url_req = demo_req)
})
