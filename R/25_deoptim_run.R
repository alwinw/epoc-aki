# Cr gradient only model
set.seed(8)
grad_only_model <- deoptim_search(
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = NULL,
  add_gradient_predictor = 1,
  stepwise = FALSE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  )
)

# Cr change model
set.seed(8)
change_only_model <- deoptim_search(
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = "del_cr",
  add_gradient_predictor = NULL,
  stepwise = FALSE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  )
)

# Cr percentage change model
set.seed(8)
per_only_model <- deoptim_search(
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = "per_cr_change",
  add_gradient_predictor = NULL,
  stepwise = FALSE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  )
)

# Multivariate model
set.seed(8)
multi_model <- deoptim_search(
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + PVD + Chronic_liver_disease" # HT excluded
  ),
  cr_predictors = c("cr", "per_cr_change"),
  add_gradient_predictor = 1,
  stepwise = TRUE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  )
)
