# ---- aki_grid_wrapper ----
aki_grid_wrapper <- function(
  grid_in,
  outcome_var = "AKI_ICU",
  baseline_predictors = "",
  cr_predictors = "",
  add_gradient_predictor = NULL,
  stepwise = FALSE,
  k = "mBIC",
  cluster = FALSE
) {
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
    ch_min <- par[1] - par[2] / 2
    ch_max <- par[1] + par[2] / 2
    aki_min <- par[3]
    aki_max <- par[3] + par[4]
    model = aki_dev_wrapper(
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
  grid_tidy = tibble(bind_rows(grid_out))

  return(grid_tidy)
}

grid_in <- expand.grid(list(
  p1 = seq(3, 10, by = 3),
  p2 = seq(1, 3, by = 3),
  p3 = seq(8, 12, by = 3),
  p4 = seq(3, 48, by = 20)
))

grid_multi_model <- aki_grid_wrapper(
  grid_in,
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  add_gradient_predictor = 1,
  stepwise = TRUE,
  k = "mBIC",
  cluster = TRUE
)
