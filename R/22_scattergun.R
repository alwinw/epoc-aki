# ---- goals ----
# cr_ch (upper):
# ("hrs", "goodness")
# (0, 0), (2, 4), (4, 10), (6, 10), (7, 9), (10, 2)
# t_aki (upper):
# ("hrs", "goodness")
# (12, 4), (24, 10), (48, 10), (72, 4)
# Clinical ease
# No APACHE II/III as may be unavailable when pt is admitted to ICU
# Autoamted alarm ease
# No baseline_cr or co-morbidities

# https://en.wikipedia.org/wiki/Metaheuristic

# ---- cr_ch_only ----
# set.seed(8)
n <- 100
optim_in <- rbind(
  cbind(
    runif(n, 3, 9),
    runif(n, 1, 3),
    runif(n, 8, 12),
    runif(n, 4, 48)
  )
)
heuristic_calc <- function(AUC, per_admin_in) {
  return(AUC)
}
optim_only_model <- aki_optim_wrapper(
  optim_in,
  outcome_var = "AKI_2or3",
  baseline_predictors = "",
  cr_predictors = "",
  add_gradient_predictor = 1,
  lower = c(3, 0.1, 8, 3),
  upper = c(10, 3, 12, 48),
  cluster = TRUE
)
kable(head(optim_only_model$optim_summary, 20))
write.csv(
  optim_only_model$optim_tidy,
  paste0("optim_only ", format(Sys.time(), "%Y-%m-%d %H-%M-%S"), ".csv"),
  row.names = FALSE
)

# ---- multi ----
# set.seed(8)
n <- 100
optim_in <- rbind(
  cbind(
    runif(n, 3, 10),
    runif(n, 1, 3),
    runif(n, 8, 12),
    runif(n, 3, 48)
  )
)
heuristic_calc <- function(AUC, per_admin_in) {
  return((
    1 / 2 + 1 / 2 * tanh(10.9861 * (AUC - 0.90 + 0.1)) +
      1 / 2 + 1 / 2 * tanh(10.9861 * (per_admin_in - 0.40 + 0.1))
  ) / 2)
}
optim_multi_model <- aki_optim_wrapper(
  optim_in,
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  add_gradient_predictor = 1,
  stepwise = FALSE,
  k = "mBIC",
  lower = c(3, 0.1, 8, 3),
  upper = c(10, 3, 12, 48),
  cluster = TRUE
)
kable(head(optim_multi_model$optim_summary, 20))
write.csv(
  optim_multi_model$optim_tidy,
  paste0("optim_multi ", format(Sys.time(), "%Y-%m-%d %H-%M-%S"), ".csv"),
  row.names = FALSE
)


# ---- example_model_1 ----
cat("del_t_ch_hr_range = c(4.55, 7.18)\ndel_t_aki_hr_range = c(9.39, 12.72)")
scatter_example <- aki_dev_wrapper(
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(4.55, 7.18),
  del_t_aki_hr_range = c(9.39, 12.72),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(scatter_example$model, print = FALSE, digits = c(2, 3))$regressionTable,
  align = c("l", "c", "c", "c", "c")
)
kable(summarise_cutpoint(scatter_example), align = c("l", "r"))


# ---- example_model_2 ----
cat("del_t_ch_hr_range = c(4.55, 7.18)\ndel_t_aki_hr_range = c(9.39, 12.72)")
scatter_example <- aki_dev_wrapper(
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(4.55, 7.18),
  del_t_aki_hr_range = c(9.39, 12.72),
  add_gradient_predictor = 1,
  all_data = TRUE,
  stepwise = TRUE,
  k = "mBIC",
  analysis_data = analysis_df
)
kable(publish(scatter_example$model, print = FALSE, digits = c(2, 3))$regressionTable,
  align = c("l", "c", "c", "c", "c")
)
kable(summarise_cutpoint(scatter_example), align = c("l", "r"))
