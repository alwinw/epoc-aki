score_analysis <- epoc_aki$analysis %>%
  mutate(
    cr_gradient = if_else(del_cr >= 1 * del_t_ch_hr, 1, 0),
    ARBOC_Score = score_predictor(PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient)
  )

score_model <- deoptim_search(
  analysis_data = score_analysis,
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "ARBOC_Score"
  ),
  cr_predictors = NULL,
  add_gradient_predictor = NULL,
  first_cr_only = FALSE,
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
  ),
  override = c(4.9, 1.8, 8.7, 16.9),
  print = FALSE
)
