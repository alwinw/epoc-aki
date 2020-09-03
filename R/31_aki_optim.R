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
  cluster = FALSE
) {
  optim_in_list <- asplit(optim_in, 1)
  if(cluster) {
    cl <- makeCluster(detectCores() - 1)
    invisible(clusterEvalQ(cl, library("dplyr")))
    invisible(clusterEvalQ(cl, library("cutpointr")))
    clusterExport(cl, c(
      "analysis_df", "aki_dev_wrapper", "heuristic_calc",
      "outcome_var", "baseline_predictors", "cr_predictors",
      "add_gradient_predictor", "lower", "upper"),
    envir=environment())
  } else {
    cl <- NULL
  }

  optim_out <- pblapply(optim_in_list, function(par) {
    optim_cl <- optim(par, function(x) {
        ch_min = x[1] - x[2]/2
        ch_max = x[1] + x[2]/2
        aki_min = x[3]
        aki_max = x[3] + x[4]
        output = aki_dev_wrapper(
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
  if(cluster) {
    stopCluster(cl)
  }
  optim_out_df <- bind_rows(optim_out)

  optim_tidy <- optim_out_df %>%
    arrange(value) %>%
    mutate(
      ch_hr_lower = p1 - p2/2,
      ch_hr_upper = p1 + p2/2,
      aki_hr_lower = p3,
      aki_hr_upper = p3 + p4
    ) %>%
    rowwise() %>%
    do(data.frame(
      .,
      aki_dev_wrapper(
        outcome_var = outcome_var,
        baseline_predictors = baseline_predictors,
        cr_predictors = baseline_predictors,
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
    select(-p1:-p4, -fevals:-convergence, -value)

  optim_summary <- optim_tidy %>%
    mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
    mutate_if(is.integer, as.character)

  return(list(
    optim_tidy = optim_tidy,
    optim_summary = optim_summary
  ))
}
