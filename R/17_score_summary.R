score_analysis <- epoc_aki$analysis %>%
  mutate(
    cr_gradient = if_else(del_cr >= 1 * del_t_ch_hr, 1, 0),
    ARBOC_score = PCs_cardio + Vasopressor + 3 * Chronic_liver_disease + cr_gradient,
    ARBOC_pred = score_predictor(PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient)
  ) %>%
  select(
    AKI_2or3, AKI_ICU, Cr_defined_AKI_2or3, Cr_defined_AKI, Olig_defined_AKI_2or3, Olig_defined_AKI,
    AdmissionID, UR_number, del_t_ch_hr, del_t_aki_hr, del_cr, cr_gradient,
    ARBOC_score, ARBOC_pred
  )

score_model <- deoptim_search(
  analysis_data = score_analysis,
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "ARBOC_pred"
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


# The following is for ALL Cr changes
cutpoint_value <- 2

score_model$optim_model$data %>%
  mutate(
    pred = if_else(ARBOC_score >= cutpoint_value, 1, 0),
    tp = if_else(pred == 1 & AKI_2or3 == 1, 1, 0),
    tn = if_else(pred == 0 & AKI_2or3 == 0, 1, 0),
    fp = if_else(pred == 1 & AKI_2or3 == 0, 1, 0),
    fn = if_else(pred == 0 & AKI_2or3 == 1, 1, 0)
  ) %>%
  summarise(
    tp = sum(tp),
    fn = sum(fn),
    fp = sum(fp),
    tn = sum(tn),
  ) %>%
  mutate(
    sensitivity = tp / (tp + fn),
    specificity = tn / (tn + fp),
    ppv = tp / (tp + fp),
    npv = tn / (tn + fn),
    plr = plr(tp, fp, tn, fn)[1],
    nlr = nlr(tp, fp, tn, fn)[1],
    cutpoint_value = cutpoint_value
  )

values = as.list(0:6)

lapply(values, function(cutpoint_value){
  score_model$optim_model$data %>%
    mutate(
      pred = if_else(ARBOC_score >= cutpoint_value, 1, 0),
      tp = if_else(pred == 1 & AKI_2or3 == 1, 1, 0),
      tn = if_else(pred == 0 & AKI_2or3 == 0, 1, 0),
      fp = if_else(pred == 1 & AKI_2or3 == 0, 1, 0),
      fn = if_else(pred == 0 & AKI_2or3 == 1, 1, 0)
    ) %>%
    summarise(
      tp = sum(tp),
      fn = sum(fn),
      fp = sum(fp),
      tn = sum(tn),
    ) %>%
    mutate(
      sensitivity = tp / (tp + fn),
      specificity = tn / (tn + fp),
      ppv = tp / (tp + fp),
      npv = tn / (tn + fn),
      plr = plr(tp, fp, tn, fn)[1],
      nlr = nlr(tp, fp, tn, fn)[1],
      ARBOC_score = cutpoint_value
    )
}) %>% 
  bind_rows() %>% 
  write.csv("table5.csv", row.names = FALSE)
