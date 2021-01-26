# ---- wrapper functions ----

tanh_penalty <- function(x, c, d, s) 1 / 2 + 1 / 2 * tanh(s / d * atanh(0.8) * (x - c))

heuristic_penalty <- function(summary) {
  0 +
    (1 - summary$AUC) * 3 +
    (1 - summary$per_admin_in) * 2 +
    (1 - summary$per_admin_pos) * 3 +
    tanh_penalty(summary$ch_hr_lower, 9, 1, 1) +
    tanh_penalty(summary$ch_hr_upper, 9, 1, 1) +
    tanh_penalty(summary$aki_hr_upper, 15, 15, -1) +
    tanh_penalty(summary$aki_hr_upper, 60, 10, 1) +
    grepl("APACHE_II", summary$glm_model) * 2 +
    grepl("APACHE_III", summary$glm_model) * 2 +
    grepl("Baseline_Cr", summary$glm_model) +
    grepl("cr", summary$glm_model)
  # other
}

heuristic_wrapper <- function(
                              x,
                              penalty_fn = function(x) x$penalty,
                              return_fn = function(x) x,
                              ...) {
  summary <- aki_dev_wrapper(
    del_t_ch_hr_range = c(x[1] - x[2] / 2, x[1] + x[2] / 2),
    del_t_aki_hr_range = c(x[3], x[3] + x[4]),
    ...
  )
  summary$penalty <- penalty_fn(summary)
  return(return_fn(summary))
}
# summary <- heuristic_wrapper(
#   c(6, 1, 6, 12),
#   outcome_var = "AKI_ICU",
#   penalty_fn = function(summary) 1 - summary$AUC,
#   return_fn = function(summary) summary
# )

deoptim_wrapper <- function(
                            lower, upper,
                            itermax, ...,
                            return_fn = function(summary) summary$penalty,
                            fnMap = function(x) round(x, 1)) {
  result <- DEoptim(
    heuristic_wrapper,
    lower = lower,
    upper = upper,
    control = DEoptim.control(
      itermax = itermax,
      NP = 100,
      reltol = 1e-5,
      parallelType = 1,
      packages = c("dplyr", "cutpointr"),
      parVar = c("analysis_df", "aki_dev_wrapper", "heuristic_penalty", "tanh_penalty")
    ),
    ...,
    return_fn = return_fn,
    fnMap = fnMap
  )
  return(list(
    bestmem = heuristic_wrapper(result$optim$bestmem, ...),
    result = result
  ))
}

# ---- baseline ----
baseline_all <- aki_dev_wrapper(
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = NULL,
  add_gradient_predictor = NULL,
  all_data = TRUE,
  analysis_data = baseline_df
)
publish(baseline_all$model, print = FALSE, digits = c(2, 3))$regressionTable


baseline_sig <- aki_dev_wrapper(
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = NULL,
  add_gradient_predictor = NULL,
  stepwise = TRUE,
  k = "AIC",
  all_data = TRUE,
  analysis_data = baseline_df
)
baseline_sig$summary
publish(baseline_sig$model, print = FALSE, digits = c(2, 3))$regressionTable

# ---- cr_ch_only ----
set.seed(8)
cr_ch_optim <- deoptim_wrapper(
  lower = c(4, 0.5, 3, 1),
  upper = c(6, 6, 12, 72),
  itermax = 50,
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = NULL,
  add_gradient_predictor = 1,
  penalty_fn = heuristic_penalty
)
cr_ch_optim$bestmem

cr_ch_bestmem <- heuristic_wrapper(cr_ch_optim$result$optim$bestmem,
  outcome_var = "AKI_2or3",
  baseline_predictors = "",
  cr_predictors = "",
  add_gradient_predictor = 1,
  all_data = TRUE
)
publish(cr_ch_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable


# ---- multi ----
set.seed(8)
multi_optim <- deoptim_wrapper(
  lower = c(4, 0.5, 3, 1),
  upper = c(6, 6, 12, 72),
  itermax = 50,
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = NULL,
  add_gradient_predictor = 1,
  penalty_fn = heuristic_penalty
)
multi_optim$bestmem

multi_bestmem <- heuristic_wrapper(multi_optim$result$optim$bestmem,
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  add_gradient_predictor = 1,
  stepwise = TRUE,
  k = "mBIC",
  all_data = TRUE
)
publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
multi_bestmem$summary
