#' Create default model configurations
#' @param model_weights_df data.frame of model weights
#' @param W_graph graph used for INLA model
#' @param pred_vars formatted predictor variable names
#' @param inla_hyper list of inla hyper parameters. See Details
#' @param ranger_hyper list of ranger hyper parameters. See Details
#' @returns list of model configurations for each of five models
#' @export
create_model_configs <- function(model_weights_df, W_graph, pred_vars,
                                 inla_hyper = list("prec.unstruct" = c(1, 5e-4),
                                                   "prec.spatial" = c(1, 5e-4),
                                                   "prec.timerw1" = c(1,0.01)),
                                 ranger_hyper = list("mtry" = NULL,
                                                     "min.node.size" = NULL,
                                                     "num.trees" = 500)
                                 ){
  model_weights <- model_weights_df$weight
  names(model_weights) <-  model_weights_df$model

  model_configs <- list()
  model_configs$inla <- list(reff_var = NULL, pred_vars = pred_vars,
                            hyper_priors = inla_hyper,
                            W_orgUnit = W_graph,
                            sample_pi = TRUE,
                            weight = model_weights["inla"])
  model_configs$glm_nb <- list(pred_vars = pred_vars,
                               weight = model_weights["glm_nb"])
  model_configs$ranger <- list(pred_vars = pred_vars,
                               hyper_control = ranger_hyper,
                               weight = model_weights["ranger"])
  model_configs$arimax <- list(pred_vars = pred_vars,
                               log_trans = TRUE,
                               weight = model_weights["arimax"])
  model_configs$naive <- list(group_vars = c("month_season", "orgUnit"),
                              weight = model_weights["naive"])

  #if any weight is 0, do not fit that model
  zero_weights <- names(model_weights)[which(model_weights==0)]
  model_configs[names(model_configs) %in% zero_weights] <- NULL

  return(model_configs)
}
