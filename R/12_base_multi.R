# ---- baseline_model ----
baseline_df <- admission_data %>%
  filter(Excl_criteria_ok == 1) %>%
  filter(Event != "Neither") %>%   # TODO In the future, this should not be an exclusion
  select(
    `UR number`:Admission, Pt_Study_nos, Event,
    Age, APACHE_II, APACHE_III, Baseline_Cr, PCs_cardio, Vasopressor:Chronic_liver_disease,
    AKI_ICU, AKI_stage
  ) %>%
  mutate(
    APACHE_II  = if_else(APACHE_II  == 0, NA_real_, APACHE_II),
    APACHE_III = if_else(APACHE_III == 0, NA_real_, APACHE_III)
  ) %>%
  group_by(AKI_ICU) %>%
  mutate(
    APACHE_II  = if_else(is.na(APACHE_II),  median(APACHE_II,  na.rm = TRUE),  APACHE_II),
    APACHE_III = if_else(is.na(APACHE_III), median(APACHE_III,na.rm = TRUE), APACHE_III)
  ) %>%   # FIXME Replace with REAL data
  ungroup()


baseline_model <- glm(
  AKI_ICU ~ Age + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio +
    Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease,
  family = "binomial", data = baseline_df)

summary(baseline_model)
kable(publish(baseline_model, print=FALSE)$regressionTable)

baseline_df$model <- predict(baseline_model, type = "response")
baseline_cut <- cutpointr(
  baseline_df, model, AKI_ICU,
  use_midpoints = TRUE,
  direction = ">=", pos_class = 1, neg_class = 0,
  method = maximize_metric, metric = youden)


# ---- auroc_plot ----
summary(baseline_cut)
plot(baseline_cut)
# plot_metric(baseline_cut, conf_lvl = 0.9)
# plot_sensitivity_specificity(baseline_cut)

