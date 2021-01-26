# ---- aki_optim_wrapper ----
aki_optim_wrapper <- function(
                              optim_in,
                              outcome_var = "AKI_ICU",
                              baseline_predictors = "",
                              cr_predictors = "",
                              add_gradient_predictor = NULL,
                              stepwise = FALSE,
                              k = "AIC",
                              lower = c(3, 0.1, 8, 3),
                              upper = c(10, 3, 12, 48),
                              cluster = FALSE) {
  optim_in_list <- asplit(optim_in, 1)
  if (cluster) {
    cl <- makeCluster(detectCores())
    invisible(clusterEvalQ(cl, library("dplyr")))
    invisible(clusterEvalQ(cl, library("cutpointr")))
    clusterExport(cl, c(
      "analysis_df", "aki_dev_wrapper", "heuristic_calc",
      "outcome_var", "baseline_predictors", "cr_predictors",
      "add_gradient_predictor", "lower", "upper"
    ),
    envir = environment()
    )
  } else {
    cl <- NULL
  }

  optim_out <- pblapply(optim_in_list, function(par) {
    optim_cl <- optim(par, function(x) {
      ch_min <- x[1] - x[2] / 2
      ch_max <- x[1] + x[2] / 2
      aki_min <- x[3]
      aki_max <- x[3] + x[4]
      output <- aki_dev_wrapper(
        outcome_var = outcome_var,
        baseline_predictors = baseline_predictors,
        cr_predictors = cr_predictors,
        del_t_ch_hr_range = c(ch_min, ch_max),
        del_t_aki_hr_range = c(aki_min, aki_max),
        add_gradient_predictor = add_gradient_predictor,
        stepwise = stepwise,
        k = k,
        heuristic_only = TRUE,
        analysis_data = analysis_df
      )
      return(-heuristic_calc(output$AUC, output$per_admin_in))
    },
    lower = lower,
    upper = upper,
    method = "L-BFGS-B"
    )
    return(data.frame(
      p1 = optim_cl$par[1],
      p2 = optim_cl$par[2],
      p3 = optim_cl$par[3],
      p4 = optim_cl$par[4],
      value = optim_cl$value,
      fevals = optim_cl$counts[[1]],
      gevals = optim_cl$counts[[2]],
      convergence = optim_cl$convergence
    ))
  },
  cl = cl
  )
  if (cluster) {
    stopCluster(cl)
  }
  optim_out_df <- bind_rows(optim_out)

  optim_tidy <- optim_out_df %>%
    arrange(value) %>%
    mutate(
      ch_hr_lower = p1 - p2 / 2,
      ch_hr_upper = p1 + p2 / 2,
      aki_hr_lower = p3,
      aki_hr_upper = p3 + p4
    ) %>%
    rowwise() %>%
    do(data.frame(
      .,
      aki_dev_wrapper(
        outcome_var = outcome_var,
        baseline_predictors = baseline_predictors,
        cr_predictors = cr_predictors,
        del_t_ch_hr_range = c(.$ch_hr_lower, .$ch_hr_upper),
        del_t_aki_hr_range = c(.$aki_hr_lower, .$aki_hr_upper),
        add_gradient_predictor = add_gradient_predictor,
        stepwise = stepwise,
        k = k,
        heuristic_only = TRUE,
        analysis_data = analysis_df
      )
    )) %>%
    ungroup() %>%
    mutate(value = -value) %>%
    mutate(
      heuristic = heuristic_calc(AUC, per_admin_in)
    ) %>%
    select(
      p1:p4, AUC:n_admissions,
      ch_hr_lower:aki_hr_upper, value, heuristic
    )

  optim_summary <- optim_tidy %>%
    mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
    mutate_if(is.integer, as.character)

  return(list(
    optim_tidy = optim_tidy,
    optim_summary = optim_summary
  ))
}


# ---- aki_grid_wrapper ----
aki_grid_wrapper <- function(
                             grid_in,
                             outcome_var = "AKI_ICU",
                             baseline_predictors = "",
                             cr_predictors = "",
                             add_gradient_predictor = NULL,
                             stepwise = FALSE,
                             k = "mBIC",
                             cluster = FALSE) {
  grid_list <- asplit(grid_in, 1)
  if (cluster) {
    cl <- makeCluster(detectCores())
    invisible(clusterEvalQ(cl, library("dplyr")))
    invisible(clusterEvalQ(cl, library("cutpointr")))
    clusterExport(cl, c(
      "analysis_df", "aki_dev_wrapper",
      "outcome_var", "baseline_predictors", "cr_predictors",
      "add_gradient_predictor"
    ),
    envir = environment()
    )
  } else {
    cl <- NULL
  }

  grid_out <- pblapply(grid_list, function(par) {
    ch_min <- par[1] ## NOTE: THIS IS DIFFERENT to aki_optim_wrapper
    ch_max <- par[1] + par[2] ## NOTE: THIS IS DIFFERENT to aki_optim_wrapper
    aki_min <- par[3]
    aki_max <- par[3] + par[4]
    model <- aki_dev_wrapper(
      outcome_var = outcome_var,
      baseline_predictors = baseline_predictors,
      cr_predictors = cr_predictors,
      del_t_ch_hr_range = c(ch_min, ch_max),
      del_t_aki_hr_range = c(aki_min, aki_max),
      add_gradient_predictor = add_gradient_predictor,
      stepwise = stepwise,
      k = k,
      all_data = TRUE,
      analysis_data = analysis_df
    )
    return(cbind(model$summary, model$params))
  },
  cl = cl
  )
  if (cluster) {
    stopCluster(cl)
  }
  grid_tidy <- tibble(bind_rows(grid_out))

  return(grid_tidy)
}
