# ---- base_no_comorbidities ----
baseline_df <- analysis_df %>%
  select(-DateTime_Pathology_Result:-del_t_aki_hr) %>%
  unique(.)

base_min_model <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = "Age + APACHE_II + APACHE_III + Baseline_Cr",
  all_data = TRUE,
  analysis_data = baseline_df
)
kable(publish(base_min_model$model, print = FALSE, digits = c(2, 3))$regressionTable,
  align = c("l", "c", "c", "c", "c")
)
kable(summarise_cutpoint(base_min_model), align = c("l", "r"))
# plot(regressionTable(base_min_model$model))
# summary(base_min_model$cutpoint)
plot(base_min_model$cutpoint)
# plot_metric(base_sig_model$cutpoint, conf_lvl = 0.9)
rm(base_min_model)

# ---- base_with_comorbidities ----
base_all_model <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  all_data = TRUE,
  analysis_data = baseline_df
)
kable(publish(base_all_model$model, print = FALSE, digits = c(2, 3))$regressionTable,
  align = c("l", "c", "c", "c", "c")
)
kable(summarise_cutpoint(base_all_model), align = c("l", "r"))
# plot(regressionTable(base_all_model$model))
# summary(base_all_model$cutpoint)
plot(base_all_model$cutpoint)
# plot_metric(base_all_model$cutpoint, conf_lvl = 0.9)


# ---- base_significant_only ----
# Consider using model selection criteria e.g. AIC
base_sig_model <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = "PCs_cardio + Vasopressor + HT + Chronic_liver_disease",
  all_data = TRUE,
  analysis_data = baseline_df
)
kable(publish(base_sig_model$model, print = FALSE, digits = c(2, 3))$regressionTable,
  align = c("l", "c", "c", "c", "c")
)
kable(summarise_cutpoint(base_sig_model), align = c("l", "r"))
# plot(regressionTable(base_sig_model$model))
# summary(base_sig_model$cutpoint)
plot(base_sig_model$cutpoint)
# plot_metric(base_sig_model$cutpoint, conf_lvl = 0.9)
rm(base_sig_model)
rm(baseline_df)
