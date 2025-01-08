## code to prepare `demo_polygon` dataset goes here

library(sf)
demo_polygon <- st_read("data-raw/demo_polygon.gpkg")

usethis::use_data(demo_polygon, overwrite = TRUE)
