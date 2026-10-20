#INLA defaults
INLA_HYPER_DEFAULT <- list("prec.spatial" = c(1, 0.01),
                          "phi.spatial" = c(0.5,0.5),
                          "prec.timerw1" = c(1,0.01))
INLA_MAX_TRY_DEFAULT <- 2
INLA_MAX_TIME_DEFAULT <- 300
INLA_SAMPLEPI_DEFAULT <- TRUE


ncores_default <- function() {
  max(ceiling(parallel::detectCores() / 2), 1L, na.rm = TRUE)
}
