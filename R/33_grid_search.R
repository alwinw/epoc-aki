grid_in <- expand.grid(list(
  p1 = seq(3, 10, by = 1),
  p2 = seq(1, 3, by = 1),
  p3 = seq(8, 12, by = 3),
  p4 = seq(3, 48, by = 5)
))

grid_only_model <- aki_grid_wrapper(
  grid_in,
  outcome_var = "AKI_ICU",
  baseline_predictors = "",
  cr_predictors = "",
  add_gradient_predictor = 1,
  stepwise = FALSE,
  k = "mBIC",
  cluster = TRUE
)
write.csv(
  grid_only_model,
  paste0("grid_only ", format(Sys.time(), "%Y-%m-%d %H-%M-%S"), ".csv"),
  row.names = FALSE
)

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
write.csv(
  grid_multi_model,
  paste0("grid_multi ", format(Sys.time(), "%Y-%m-%d %H-%M-%S"), ".csv"),
  row.names = FALSE
)
