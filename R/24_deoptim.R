# ---- wrapper functions ----

tanh_penalty <- function(x, c, d, s) 1 / 2 + 1 / 2 * tanh(s / d * atanh(0.8) * (x - c))

heuristic_penalty <- function(summary) {
  0 +
    (1 - summary$AUC) * 5 +
    (1 - summary$per_admin_in) * 2 +
    (1 - summary$per_admin_pos) * 3 +
    tanh_penalty(summary$ch_hr_lower, 9, 1, 1) +
    tanh_penalty(summary$ch_hr_upper, 9, 1, 1) +
    tanh_penalty(summary$aki_hr_upper, 15, 15, -1) +
    tanh_penalty(summary$aki_hr_upper, 60, 10, 1) +
    grepl("APACHE_II", summary$glm_model) * 5 +
    grepl("APACHE_III", summary$glm_model) * 5 +
    grepl("Baseline_Cr", summary$glm_model) +
    grepl("\\bcr\\b", summary$glm_model) * 3 +
    (1 - grepl("cr_gradient", summary$glm_model)) * 6
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
      NP = 160,
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
baseline_all$summary

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
publish(baseline_sig$model, print = FALSE, digits = c(2, 3))$regressionTable
baseline_sig$summary

# ---- cr_ch_only ----
set.seed(8)
cr_ch_optim <- deoptim_wrapper(
  lower = c(4, 0.5, 3, 1),
  upper = c(6, 6, 12, 72),
  itermax = 20, # 20 brief test
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = NULL,
  add_gradient_predictor = 1,
  penalty_fn = heuristic_penalty
)

cr_ch_bestmem <- heuristic_wrapper(cr_ch_optim$result$optim$bestmem,
  outcome_var = "AKI_2or3",
  baseline_predictors = "",
  cr_predictors = "",
  add_gradient_predictor = 1,
  all_data = TRUE
)
publish(cr_ch_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
cr_ch_bestmem$summary

# ---- multi ----
set.seed(8)
multi_optim <- deoptim_wrapper(
  lower = c(4, 0.5, 3, 1),
  upper = c(6, 6, 12, 72),
  itermax = 20,
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  add_gradient_predictor = 1,
  stepwise = TRUE,
  k = "mBIC",
  penalty_fn = heuristic_penalty
)

if (FALSE) multi_optim <- list(result = list(optim = list(bestmem = c(5.3, 1.6, 9.3, 30.7))))

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

multi_bestmem_all <- heuristic_wrapper(
  multi_optim$result$optim$bestmem,
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  add_gradient_predictor = 1,

  all_data = TRUE
)
publish(multi_bestmem_all$model, print = FALSE, digits = c(2, 3))$regressionTable
multi_bestmem_all$summary

# FIXME: n_admission for these below are based on multi_bestmem$data, not the real analysis_data...

multi_bestmem_aki <- heuristic_wrapper(
  multi_optim$result$optim$bestmem,
  outcome_var = "AKI_ICU",
  baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
  all_data = TRUE,
  analysis_data = multi_bestmem$data
)
publish(multi_bestmem_aki$model, print = FALSE, digits = c(2, 3))$regressionTable
multi_bestmem_aki$summary

multi_bestmem_cr <- heuristic_wrapper(
  multi_optim$result$optim$bestmem,
  outcome_var = "Cr_defined_AKI_2or3",
  baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
  all_data = TRUE,
  analysis_data = multi_bestmem$data
)
publish(multi_bestmem_cr$model, print = FALSE, digits = c(2, 3))$regressionTable
multi_bestmem_cr$summary

multi_bestmem_cr_all <- heuristic_wrapper(
  multi_optim$result$optim$bestmem,
  outcome_var = "Cr_defined_AKI",
  baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
  all_data = TRUE,
  analysis_data = multi_bestmem$data
)
publish(multi_bestmem_cr_all$model, print = FALSE, digits = c(2, 3))$regressionTable
multi_bestmem_cr_all$summary

multi_bestmem_olig <- heuristic_wrapper(
  multi_optim$result$optim$bestmem,
  outcome_var = "Olig_defined_AKI_2or3",
  baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
  all_data = TRUE,
  analysis_data = multi_bestmem$data
)
publish(multi_bestmem_olig$model, print = FALSE, digits = c(2, 3))$regressionTable
multi_bestmem_olig$summary

multi_bestmem_olig_all <- heuristic_wrapper(
  multi_optim$result$optim$bestmem,
  outcome_var = "Olig_defined_AKI",
  baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
  all_data = TRUE,
  analysis_data = multi_bestmem$data
)
publish(multi_bestmem_olig_all$model, print = FALSE, digits = c(2, 3))$regressionTable
multi_bestmem_olig_all$summary

multi_bestmem_baseline <- heuristic_wrapper(
  multi_optim$result$optim$bestmem,
  outcome_var = "AKI_2or3",
  baseline_predictors = gsub(".*~ | \\+ cr_gradient", "", multi_bestmem$summary$glm_model),
  all_data = TRUE,
  analysis_data = multi_bestmem$data
)
publish(multi_bestmem_baseline$model, print = FALSE, digits = c(2, 3))$regressionTable
multi_bestmem_baseline$summary
