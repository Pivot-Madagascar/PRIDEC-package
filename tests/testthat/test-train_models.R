test_that("train model works", {
  #don't run this on CRAN, it will take ages
  skip('skip train_models because it takes a long time to run')
  data(demo_malaria)
  data(demo_polygon)

  prep_output <- prep_data(raw_data = demo_malaria[demo_malaria$period>201903,],
                           y_var = "n_case",
                           lagged_vars = c("pev", "rain_mm", "temp_c"),
                           scaled_vars = c("pev", "rain_mm", "temp_c",
                                           "wealth_index", "elevation",
                                           "time_to_district"),
                           lag_n = 3,
                           graph_poly = demo_polygon)
  #for debugging
  train_models(prep_output,
               models = c("naive", "inla", "arimax", "glm_nb", "ranger"),
               y_var = "n_case",
               pred_vars = c("pev_lagsc", "rain_mm_lagsc", "temp_c_lagsc",
                             "wealth_indexsc", "elevationsc",
                             "time_to_districtsc"),
               id_vars = c("orgUnit", "date"),
               results_dir = "scratch/demo_trainModelResults",
               model_configs = NULL)


})
