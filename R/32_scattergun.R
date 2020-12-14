# =1/2 + 1/2*TANH(10.9861*($E2 - 0.8))
# =1/2 + 1/2*TANH(10.9861*($F2 - 0.3))
# =1/2 + 1/2*TANH(-1.0986*($I2 - 8))
# =1/2 + 1/2*TANH(0.0549*($K2 - 28))
#
# s/d * atanh(0.8)

# ---- cr_ch_only ----
# set.seed(8)
n <- 10000
optim_in <- rbind(
  cbind(
    runif(n, 3, 10),
    runif(n, 1, 3),
    runif(n, 8, 12),
    runif(n, 3, 48)
  )
)
heuristic_calc <- function(AUC, per_admin_in) {
  return(AUC)
}
optim_only_model <- aki_optim_wrapper(
  optim_in,
  outcome_var = "AKI_ICU",
  baseline_predictors = "",
  cr_predictors = "",
  add_gradient_predictor = 1,
  lower = c(3, 0.1, 8, 3),
  upper = c(10, 3, 12, 48)
)
kable(head(optim_only_model$optim_summary, 20))
write.csv(
  optim_only_model$optim_tidy,
  paste0("optim_only ", format(Sys.time(), "%Y-%m-%d %H-%M-%S"), ".csv"),
  row.names = FALSE
)

# ---- multi ----
# set.seed(8)
n <- 10000
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
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age + APACHE_II + APACHE_III + Baseline_Cr",
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
