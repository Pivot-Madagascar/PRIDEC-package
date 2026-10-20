create_inla_data_basic <- function(cv_set, y_var, pred_vars, id_vars){


  # ------ set up data ------
  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars,
                                                                  "org_ID",
                                                                  "month_season", "month_num"),
                             remove_NA = FALSE)
  this_analysis <- cv_clean$analysis
  this_analysis$observed <- this_analysis$y_obs

  this_assess <- cv_clean$assess
  #will predict on things that are NA
  this_assess$observed <- this_assess$y_obs
  this_assess$y_obs <- as.numeric(NA)

  both_datasets <- dplyr::bind_rows(this_analysis, this_assess)
  both_datasets$idx <- 1:nrow(both_datasets)

  return(both_datasets)
}

create_inla_data_vi <- function(cv_set, y_var, pred_vars, id_vars, nsims){
  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars, "org_ID",
                                                                  "month_season", "month_num"),
                             remove_NA = FALSE)
  this_analysis <- cv_clean$analysis
  this_analysis$observed <- this_analysis$y_obs
  this_analysis$nsim <- 0
  this_analysis$perm_var = "original"

  #---- create variable importance data ---------
  permute_vars <- pred_vars


  permute_data <- purrr::map(1:length(permute_vars), function(var_i,...){
    this_var_permute <- purrr::map(1:nsims, function(sim_i,...){
      this_permute_df <- this_analysis |>
        dplyr::mutate(perm_var = permute_vars[[var_i]],
                      nsim = sim_i)
      this_permute_df[permute_vars[[var_i]]] <- sample(this_permute_df[[permute_vars[[var_i]]]],
                                                       size = nrow(this_permute_df),
                                                       replace = FALSE)
      return(this_permute_df)
    }) |>
      dplyr::bind_rows() #end nsims simulation of permutation for one variable

  }) |>
    dplyr::bind_rows() #end map over all pred_vars

  permute_data <- permute_data |>
    dplyr::mutate(y_obs = NA) |>
    dplyr::select(dplyr::all_of(c("y_obs", "observed", id_vars,
                                  "org_ID",
                                  "month_season", "month_num",
                                  pred_vars, "nsim", "perm_var"))) |>
    dplyr::mutate(dataset = "varImp")

  both_datasets <- dplyr::bind_rows(this_analysis, permute_data)
  return(both_datasets)
}

create_inla_data_counterfactual <- function(cv_set, y_var, pred_vars, id_vars, constant_org, constant_date){
  cv_clean <- get_cv_subsets(cv_set, y_var = y_var, pred_vars = c(pred_vars, id_vars, "org_ID",
                                                                  "month_season", "month_num"),
                             remove_NA = TRUE)
  this_analysis <- cv_clean$analysis
  this_analysis$observed <- this_analysis$y_obs

  counter_one <- subset(this_analysis, this_analysis$orgUnit == constant_org & this_analysis$date == constant_date)
  counter_one$y_obs <- NA

  #throw error message if the constant_org and constant_date aren't working
  if(nrow(counter_one)!=1){
    cli::cli_abort(c(
      "The combination of {.var constant_org} and {.var constant_date} has an NA in the y_var column.",
      "Please supply new values for these constants."
    ))
  }

  #loop over pred_vars to create counter-data for each one
  counter_vars <- c("org_ID", "month_season", "month_num", pred_vars)

  counter_data <- purrr::map(1:length(counter_vars), function(i){
    this_var_name <- counter_vars[i]
    #create a grid to fit
    var_class <- class(this_analysis[[this_var_name]])
    if(var_class == "numeric"){
      var_range <- range(this_analysis[this_var_name], na.rm = TRUE)
      var_grid <- seq(var_range[1], var_range[2], length.out = 50)
      if(nrow(unique(this_analysis[this_var_name]))<10){
        var_grid <- unique(this_analysis[[this_var_name]])
      }
    } else {
      var_grid <- unique(this_analysis[[this_var_name]])
    }
    this_counter <- counter_one[rep(1, length(var_grid)),]
    this_counter[this_var_name] <- var_grid
    this_counter$counter_variable <- this_var_name
    return(this_counter)
  }) |> dplyr::bind_rows()

  both_datasets <- dplyr::bind_rows(mutate(this_analysis, dataset = "analysis"),
                                    mutate(counter_data, dataset = "counter"))
  return(both_datasets)
}

diagnose_inla_data <- function(data, W_orgUnit = NULL, pred_vars,
                               org_var = "org_ID", month_var = "month_season",
                               min_months_per_group = 3) {
  tol <- 1e-8
  issues <- list()

  # 1. org_IDs in data missing from the graph -> hard error in inla()
  if (!is.null(W_orgUnit)) {
    data_orgs  <- na.omit(unique(as.character(data[[org_var]])))
    graph_orgs <- rownames(W_orgUnit)
    missing_from_graph <- setdiff(data_orgs, graph_orgs)
    if (length(missing_from_graph) > 0) {
      issues$missing_from_graph <- missing_from_graph
      cli::cli_alert_danger("org_IDs in data but not in W_orgUnit: {paste(missing_from_graph, collapse=', ')}")
    }

    # 2. islands in the full graph
    g <- INLA::inla.read.graph(W_orgUnit)
    if (g$cc$n > 1) {
      issues$islands <- g$cc$n
      cli::cli_alert_danger("Graph has {g$cc$n} disconnected components (islands).")
    }

    # 3. rank / near-singularity check on the ICAR structure matrix actually
    #    used by bym2 -- catches near-degenerate substructure even without
    #    full islands, e.g. a node with only one active neighbour after
    #    subsetting, or numerically weak connectivity
    adj <- W_orgUnit != 0
    deg <- Matrix::rowSums(adj)
    Q_icar <- Matrix::Diagonal(x = deg) - adj
    ev <- tryCatch(
      eigen(as.matrix(Q_icar), symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) NA
    )
    if (!all(is.na(ev))) {
      n_near_zero <- sum(ev < 1e-8)
      # exactly 1 near-zero eigenvalue per connected component is expected
      # and handled internally by bym2's rank correction; MORE than that
      # signals real degeneracy
      if (n_near_zero > g$cc$n) {
        issues$icar_rank_deficient <- n_near_zero
        cli::cli_alert_danger(
          "ICAR structure matrix has {n_near_zero} near-zero eigenvalues, expected {g$cc$n}. Possible extra degeneracy."
        )
      }
    }

    # 4. any node with degree 0 (isolated within its own row, distinct from
    #    full multi-node islands above -- catches a lone singleton dropped
    #    out during subsetting)
    zero_degree <- graph_orgs[deg == 0]
    if (length(zero_degree) > 0) {
      issues$zero_degree_nodes <- zero_degree
      cli::cli_alert_warning("Nodes with zero neighbours: {paste(zero_degree, collapse=', ')}")
    }
  }

  # 5. per-org_ID temporal coverage for the cyclic grouped rw1 --
  #    thin coverage in THIS subset (not necessarily the full dataset)
  #    can make that group's block near-degenerate
  coverage <- tapply(data[[month_var]], data[[org_var]], function(x) length(unique(x)))
  thin_orgs <- names(coverage)[coverage < min_months_per_group]
  if (length(thin_orgs) > 0) {
    issues$thin_month_coverage <- coverage[thin_orgs]
    cli::cli_alert_warning(
      "org_IDs with < {min_months_per_group} distinct months in this subset: {paste(thin_orgs, collapse=', ')}"
    )
  }

  # 6. org_IDs with only 1 row total in this subset -- can't inform
  #    both the rw1xiid group block and contribute meaningfully
  n_per_org <- table(data[[org_var]])
  singleton_orgs <- names(n_per_org)[n_per_org == 1]
  if (length(singleton_orgs) > 0) {
    issues$singleton_orgs <- singleton_orgs
    cli::cli_alert_warning("org_IDs with only 1 observation in this subset: {paste(singleton_orgs, collapse=', ')}")
  }

  # 7. dropped/unused factor levels -- can silently change group indexing
  #    between runs on different subsets
  for (v in c(org_var, month_var)) {
    if (is.factor(data[[v]])) {
      unused <- setdiff(levels(data[[v]]), unique(as.character(data[[v]])))
      if (length(unused) > 0) {
        issues[[paste0("unused_levels_", v)]] <- unused
        cli::cli_alert_info("{v} has unused factor levels in this subset: {paste(unused, collapse=', ')}")
      }
    }
  }

  #--- predictor variables ------------------
  predvar_issues <- list()
  for (v in pred_vars) {
    x <- data[[v]]
    if (is.numeric(x)) {
      v_var <- stats::var(x, na.rm = TRUE)
      if (is.na(v_var) || v_var < tol) {
        predvar_issues[[v]] <- "zero or near-zero variance in this subset"
        cli::cli_alert_danger("Predictor '{v}' has near-zero variance in this subset (var = {signif(v_var, 4)}).")
      }
    } else {
      # factor/character: check for singleton or all-identical levels
      tab <- table(x)
      if (length(tab) < 2) {
        predvar_issues[[v]] <- "only one level present in this subset"
        cli::cli_alert_danger("Predictor '{v}' has only one level in this subset: {names(tab)}.")
      }
    }
  }

  # rank check on the full fixed-effects design matrix, catching
  # multicollinearity that a pairwise check could miss
  X <- stats::model.matrix(stats::reformulate(pred_vars), data = data)
  qr_X <- qr(X)
  if (qr_X$rank < ncol(X)) {
    deficient <- ncol(X) - qr_X$rank
    issues$design_matrix_rank_deficient <- deficient
    cli::cli_alert_danger("Fixed-effects design matrix is rank-deficient by {deficient} column(s) in this subset.")
  }

  # pairwise collinearity between predictors (|r| > 0.9)
  #  (skips predictors already flagged above, e.g. zero variance, where cor() is undefined)
  ok_vars <- setdiff(pred_vars[vapply(data[pred_vars], is.numeric, logical(1))], names(predvar_issues))
  if (length(ok_vars) > 1 && check_collinearity(data[ok_vars])) {
    predvar_issues$collinear_predictors <- "pairwise correlation > 0.9 between predictors"
  }

  if (length(issues) == 0) {
    cli::cli_alert_success("No structural issues detected in this data.")
  }

  if (length(predvar_issues) == 0) {
    cli::cli_alert_success("No predictor variable issues detected in this data.")
  }


  invisible(list(issues, predvar_issues))
}
