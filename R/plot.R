#' Plot predictions and PI ribbon with ggplot2
#' @param pred_df data.frame of prediction intervals
#' @param quantile_ribbon quantile levels to use for ribbon
#'
#' @returns ggplot object
#' @export
plot_predictions <- function(pred_df, quantile_ribbon = c(0.05, 0.95)){

  pred_df |>
    dplyr::filter(.data$quant_long %in% paste0("quant_", c(0.5, quantile_ribbon))) |>
    dplyr::mutate(quant_label = dplyr::case_when(
      .data$quantile_level == 0.5 ~ "median",
      .data$quantile_level < 0.5 ~ "lowPI",
      .data$quantile_level >0.5 ~ "uppPI"
    )) |>
    dplyr::select(-all_of(c("quantile_level", "quant_long"))) |>
    tidyr::pivot_wider(names_from = "quant_label", values_from = "predicted") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lowPI, ymax = .data$uppPI, fill = .data$dataset),
                           alpha = 0.2) +
      ggplot2::geom_line(ggplot2::aes(y = .data$observed), linewidth = 0.5, color ="gray50") +
      ggplot2::geom_point(ggplot2::aes(y = .data$observed), size = 0.5, color = "gray20") +
      ggplot2::geom_line(ggplot2::aes(y = .data$median, color = .data$dataset), linewidth = 0.5, alpha = 0.5) +
      ggplot2::scale_color_manual(values = c("NA", "dodgerblue")) +
      ggplot2::scale_fill_manual(values = c("NA", "navyblue")) +
      ggplot2::facet_wrap(~orgUnit, scales = "free")
}

#' Plot one counterfactual plot from models using ggplot
#' @param cf_data counterfactual data frame
#' @param var_label label used for predictor variable
#' @param y_range range of y_variable to use to limit plots
#'
#' @export
plot_counterfactual_one <-  function(cf_data, var_label, y_range = NULL){

  if(is.null(y_range)){
    y_range <- range(cf_data$yhat)
    if(y_range[1]<0) y_range[1] <-0
    if(y_range[2]>600) y_range[2] <- 600
  }

  this_variable <- gsub("sc", "", gsub("_lag3", "", cf_data$variable[1]))

  #drop some variables because they don't make sense to plot
  if(this_variable %in% c("orgUnit_X", "orgUnit_Y", "org_ID")) return(NULL)

  #for continuous variables
  if(length(unique(cf_data$var_value))>2){
  p1 <- ggplot2::ggplot(data = cf_data, ggplot2::aes(x = .data$var_value, y = .data$yhat))+
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line(color = "navyblue", linewidth = 1) +
    # geom_smooth(se = FALSE) +
    ggplot2::ylab("Nombre de cas") +
    ggplot2::coord_cartesian(ylim = c(y_range[1],y_range[2]+5)) +
    ggplot2::xlab(var_label)

  if(class(cf_data$var_value) == "character"){
    if(any(nchar(cf_data$var_value)>5)){
    p1 <- p1 +
      theme(axis.text.x = element_text(angle = 90))
    }
    if(any(nchar(cf_data$var_value)>10)){
      p1 <- p1 +
        theme(axis.text.x = element_blank())
    }
  }

  } else {
    cf_data$var_value <- c("Non", "Oui")[cf_data$var_value + 1]

    p1 <- ggplot2::ggplot(data = cf_data, ggplot2::aes(x = .data$var_value, y = .data$yhat))+
      ggplot2::geom_point(alpha = 1, size = 2) +
      ggplot2::ylab("Nombre de cas") +
      ggplot2::coord_cartesian(ylim = c(y_range[1],y_range[2]+5)) +
      ggplot2::xlab(var_label)
  }

  return(p1)
}

#' Plot multiple counterfactual plots from the list of cf_data using ggplot
#' @param cf_list list of counterfactual data.frames. Names correspond to variable labels
#' @param y_range range of y_variable to use to limit plots
#' @returns list of plots to be plotted with `patchwork::wrap_plots`
#'
#' @export
plot_counterfactual <- function(cf_list, y_range = NULL){
  #if no names, take variable from each
  if(is.null(names(cf_list))){
    names(cf_list) <-  unlist(lapply((lapply(cf_list, "[", , "variable")), unique))
  }
  pdp_plots <- purrr::map2(cf_list, names(cf_list), \(x, idx)
                           plot_counterfactual_one(cf_data = x, var_label = idx, y_range = y_range))

  all_plots <- pdp_plots |> purrr::discard(is.null)
  return(all_plots)
}
