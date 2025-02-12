#' Fit a ranger Random Forest model to a cv_set
#'
#' @param cv_set list object containing analysis and assessment data.frames
#' @param y_var string name of column of observed values
#' @param id_vars names of columns used as keys/ids in data.frames
#' @param pred_vars vector string of predictor variables to use in model
#' @param hyper_control hyperparameters to use in the model, in either list or
#'   named vector format. Defaults are ranger defaults
#' @param importance the method with which to estimate variable importance. Default = 'none' means
#'   no variable importance is estimated
#' @param quantile_levels quantile levels to use when predicted via quantile regression.
#'   Default =  c(0.01,0.025, seq(0.05,0.95, by = 0.05), 0.975, 0.99)
#' @param return_model whether or not to return ranger model object. Default  = FALSE.
#' @returns prediction intervals on analysis and assessment data or list containing model
#'   object and prediction intervals, if `return_model` = TRUE
#' @export
fit_ranger <- function(cv_set, y_var, id_vars, pred_vars,
                       hyper_control = list("mtry" = NULL, "min.node.size" = NULL, "num.trees" = 500),
                       importance = "none",
                       quantile_levels = c(0.01,0.025, seq(0.05,0.95, by = 0.05), 0.975, 0.99),
                       return_model = FALSE){

  #ensure that orgUnit and month of year are always included as predictor variables
  pred_vars <- unique(c("orgUnit", "month_season", pred_vars))

  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars), remove_NA = TRUE)
  this_analysis <- cv_clean$analysis
  this_assess <- cv_clean$assess

  #set columns to drop from formula (id variables not in pred variables)
  to_drop <- c(id_vars[!(id_vars %in% pred_vars)], "dataset")

  both_datasets <- dplyr::bind_rows(cv_clean)

  rf_mod <- ranger::ranger(y_obs ~.,
                           data = this_analysis[,!(colnames(this_analysis) %in% to_drop)],
                           mtry  = hyper_control[["mtry"]],
                           min.node.size = hyper_control[["min.node.size"]],
                           num.trees = hyper_control[["num.trees"]],
                           quantreg = TRUE,
                           importance = importance)

  rf_preds <- as.data.frame(stats::predict(object = rf_mod,
                                      data = both_datasets,
                                      type = "quantiles",
                                      quantiles = quantile_levels)) |>
    rlang::set_names(paste0("quant_", quantile_levels)) |>
    cbind(dplyr::select(both_datasets, all_of(c(id_vars, "y_obs", "dataset")))) |>
    tidyr::pivot_longer(cols = all_of(dplyr::contains("quant_")),
                        names_to = "quant_long", values_to = "predicted") |>
    dplyr::mutate(quantile_level = as.numeric(gsub("quant_","", .data$quant_long)))
  names(rf_preds)[names(rf_preds)== "y_obs"] <- "observed"

  if(return_model){
    return(list("model" = rf_mod,
                "predictions" = rf_preds))
  } else return(rf_preds)
}

#' Estimate variable importance and partial dependence plots of a ranger model
#' @inheritParams fit_ranger
#' @param var_scales data.frame containing centering and scaling parameters for variables
#' @returns list containing variable importance scores and a list of dataframes
#'   containing data for pdp plots with each element corresponding to a variable
#' @export
inv_variables_ranger <- function(cv_set, y_var, id_vars, pred_vars,
                                 hyper_control = list("mtry" = NULL, "min.node.size" = NULL, "num.trees" = 500),
                                 var_scales){
  rf_final <- fit_ranger(cv_set = cv_set,
                         y_var = y_var,
                         id_vars = id_vars,
                         pred_vars = pred_vars,
                         hyper_control = hyper_control,
                         return_model = TRUE,
                         importance = "permutation")
  rf_mod <- rf_final$model
  #---------- variable importance -------------#
  pfun_rf <- function(object, newdata) {
    stats::predict(object, newdata, type = "quantiles",
            quantiles = c(0.5))$predictions
  }
  vi_perm <- vip::vi(rf_mod)
  names(vi_perm) <- c("variable", "importance")
  vi_perm$importance <- vi_perm$importance/sum(vi_perm$importance)

  counter_df <- purrr::map(1:length(pred_vars),  function(i,...){
    this_var_name <- pred_vars[i]
    #create a grid to fit
    var_class <- class(cv_set$analysis[[this_var_name]])
    if(var_class == "numeric"){
      var_range <- range(cv_set$analysis[this_var_name], na.rm = TRUE)
      var_grid <- seq(var_range[1], var_range[2], length.out = 50)
      if(nrow(unique(cv_set$analysis[this_var_name]))<10){
        var_grid <- unique(cv_set$analysis[[this_var_name]])
      }
    } else {
      var_grid <- unique(cv_set$analysis[[this_var_name]])
    }
    var_grid <- data.frame(var_grid)
    colnames(var_grid) <- this_var_name
    pdp_df <- pdp::partial(rf_mod, train = stats::na.omit(cv_set$analysis),
                           pred.var = this_var_name,
                           pred.grid = var_grid) |>
      dplyr::mutate(variable = this_var_name)

    names(pdp_df)[names(pdp_df)==this_var_name] <- "var_valuesc"
    #add back-transformed variable
    if(stringr::str_sub(this_var_name, -2,-1)=="sc") {
      pdp_df <- pdp_df |>
        dplyr::left_join(var_scales, by = "variable") |>
        dplyr::mutate(var_value = .data$var_valuesc*.data$scale + .data$center) |>
        dplyr::select(-all_of(c("scale", "center")))
    } else {
      pdp_df <- pdp_df |>
        dplyr::mutate(var_value = .data$var_valuesc)
    }
    return(pdp_df)
  })

  return(list("var_imp" = vi_perm,
              "counter_data" = counter_df))

}

#' Tune a ranger random forest model
#' @param cvfold_list a nested list of cv_sets, each with analysis and assessment
#' @param metric metric to use to select best fit. Options are output of `eval_performance`.
#' @param tune_grid grid of parameters to tune over, with each column values for a parameter.
#'   If Default (NULL) will create one within the function.
#' @inheritParams fit_ranger

#' @returns vector of best fitting parameters
#' @export
tune_ranger <- function(cvfold_list, metric = "wape", y_var, id_vars, pred_vars,
                        tune_grid = NULL){
  if(is.null(tune_grid)){
    tune_grid <- expand.grid(m.try = unique(round(seq(2,length(pred_vars), length.out = 5))),
                             min.node.size = 3:5,
                             num.trees = c(100, 300, 500))
  }


  #map functions could be run in parallel
  test_tune <- purrr::map(1:nrow(tune_grid), function(param_i, ...){
    these_params <- tune_grid[param_i,]
    this_perf <- purrr::map(1:length(cvfold_list), function(fold_i, ...){
      fold_preds <- fit_ranger(cv_set = cvfold_list[[fold_i]],
                    y_var = y_var,
                    id_vars = id_vars,
                    pred_vars = pred_vars,
                    hyper_control = c("mtry" = these_params$m.try,
                                      "min.node.size" = these_params$min.node.size,
                                      "num.trees" = these_params$num.trees))
      fold_perf <- eval_performance(fold_preds)
      fold_perf <- fold_perf[fold_perf$dataset=="assess",]
      fold_perf$fold <- fold_i
      return(fold_perf)
    }) |>
      dplyr::bind_rows()

    these_params$metric <- mean(this_perf[[metric]])

    return(these_params)
    }) |>
    dplyr::bind_rows()

    best_fit <- test_tune[which.min(test_tune$metric),]

    return(best_fit)
}
