## code to prepare `demo_trainModelResults`  goes here

#' This dataset is the result of running `train_models` on the demo dataset,
#' so that the quarto template can be tested more easily. It saves the data under
#' scratch/demo_trainModelResults.

data(demo_malaria)
data(demo_polygon)

#12 cv sets
prep_output <- prep_data(raw_data = demo_malaria[demo_malaria$period>201803,],
                         y_var = "n_case",
                         lagged_vars = c("pev", "rain_mm", "temp_c"),
                         scaled_vars = c("pev", "rain_mm", "temp_c",
                                         "wealth_index", "elevation",
                                         "time_to_district"),
                         lag_n = 3,
                         graph_poly = demo_polygon)

train_models(prep_output,
             models = c("naive", "inla", "arimax", "glm_nb", "ranger"),
             y_var = "n_case",
             pred_vars = c("pev_lagsc", "rain_mm_lagsc", "temp_c_lagsc",
                           "wealth_indexsc", "elevationsc",
                           "time_to_districtsc"),
             id_vars = c("orgUnit", "date"),
             results_dir = "scratch/demo_trainModelResults",
             model_configs = NULL)


