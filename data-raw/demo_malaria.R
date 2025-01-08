## code to prepare `demo_malaria` dataset goes here

#' This dataset is a simulation of malaria data for 15 communes of Ifanadiana
#' district from 2015 - 2024

demo_malaria <- read.csv("data-raw/demo_malaria.csv")

usethis::use_data(demo_malaria, overwrite = TRUE)
