# ---- only_model_1 ----
cat("del_t_ch_hr_range = c(5.33, 7.25)\ndel_t_aki_hr_range = c(8, 12.66)")
only_model_1 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = "",
  cr_predictors = "",
  del_t_ch_hr_range = c(5.33, 7.25),
  del_t_aki_hr_range = c(8, 12.66),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(only_model_1$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
only_model_1$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(only_model_1)


# ---- only_model_2 ----
cat("del_t_ch_hr_range = c(5.66, 7.33)\ndel_t_aki_hr_range = c(9.33, 14.33)")
only_model_2 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = "",
  cr_predictors = "",
  del_t_ch_hr_range = c(5.66, 7.33),
  del_t_aki_hr_range = c(9.33, 14.33),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(only_model_2$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
only_model_2$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(only_model_2)


# ---- only_model_3 ----
cat("del_t_ch_hr_range = c(6.0, 7.25)\ndel_t_aki_hr_range = c(8.0, 17.66)")
only_model_3 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = "",
  cr_predictors = "",
  del_t_ch_hr_range = c(6.0, 7.25),
  del_t_aki_hr_range = c(8.0, 17.66),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(only_model_3$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
only_model_3$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(only_model_3)


# ---- multi_model_1a ----
cat("del_t_ch_hr_range = c(5.33, 8.0)\ndel_t_aki_hr_range = c(8.0, 16.0)")
multi_model_1a <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(5.33, 8.0),
  del_t_aki_hr_range = c(8.0, 16.0),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_1a$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_1a$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_1a)


# ---- multi_model_1b ----
cat("del_t_ch_hr_range = c(5.33, 8.0)\ndel_t_aki_hr_range = c(8.0, 16.0)")
multi_model_1b <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age + APACHE_III",
    "PCs_cardio + Vasopressor + Diabetes + HT + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(5.33, 8.0),
  del_t_aki_hr_range = c(8.0, 16.0),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_1b$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_1b$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_1b)


# ---- multi_model_2a ----
cat("del_t_ch_hr_range = c(6.0, 8.0)\ndel_t_aki_hr_range = c(8.0, 18.0)")
multi_model_2a <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(6.0, 8.0),
  del_t_aki_hr_range = c(8.0, 18.0),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_2a$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_2a$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_2a)


# ---- multi_model_2b ----
cat("del_t_ch_hr_range = c(6.0, 8.0)\ndel_t_aki_hr_range = c(8.0, 18.0)")
multi_model_2b <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "PCs_cardio + Vasopressor + Diabetes + Chronic_liver_disease"
  ),
  cr_predictors = "",
  del_t_ch_hr_range = c(6.0, 8.0),
  del_t_aki_hr_range = c(8.0, 18.0),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_2b$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_2b$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_2b)


# ---- multi_model_3 ----
cat("del_t_ch_hr_range = c(6.0, 7.75)\ndel_t_aki_hr_range = c(8.0, 18.33)")
multi_model_3 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age",
    "PCs_cardio + Vasopressor + Diabetes + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(6.0, 7.75),
  del_t_aki_hr_range = c(8.0, 18.33),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_3$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_3$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_3)


# ---- multi_model_4 ----
cat("del_t_ch_hr_range = c(5.75, 7.75)\ndel_t_aki_hr_range = c(9.0, 19.83)")
multi_model_4 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age",
    "PCs_cardio + Vasopressor + Diabetes + HF + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(5.75, 7.75),
  del_t_aki_hr_range = c(9.0, 19.83),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_4$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_4$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_4)


# ---- multi_model_5 ----
cat("del_t_ch_hr_range = c(5.33, 7.25)\ndel_t_aki_hr_range = c(8.0, 18.75)")
multi_model_5 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age + APACHE_III",
    "PCs_cardio + Vasopressor + Diabetes + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(5.33, 7.25),
  del_t_aki_hr_range = c(8.0, 18.75),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_5$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_5$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_5)


# ---- multi_model_6 ----
cat("del_t_ch_hr_range = c(5.75, 7.25)\ndel_t_aki_hr_range = c(8.83, 19.83)")
multi_model_6 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age",
    "PCs_cardio + Vasopressor + Diabetes + HF + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(5.75, 7.25),
  del_t_aki_hr_range = c(8.83, 19.83),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_6$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_6$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_6)


# ---- multi_model_7 ----
cat("del_t_ch_hr_range = c(6.0, 7.83)\ndel_t_aki_hr_range = c(8.75, 43.5)")
multi_model_7 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "PCs_cardio + Vasopressor + HF + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(6.0, 7.83),
  del_t_aki_hr_range = c(8.75, 43.5),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_7$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_7$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_7)


# ---- multi_model_8 ----
cat("del_t_ch_hr_range = c(6.0, 8.33)\ndel_t_aki_hr_range = c(9.25, 47.33)")
multi_model_8 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age",
    "PCs_cardio + Vasopressor + HF + HT + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(6.0, 8.33),
  del_t_aki_hr_range = c(9.25, 47.33),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_8$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_8$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_8)


# ---- multi_model_9 ----
cat("del_t_ch_hr_range = c(6.0, 8.5))\ndel_t_aki_hr_range = c(11.75, 27.16)")
multi_model_9 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Baseline_Cr",
    "PCs_cardio + Vasopressor + AF + HF + Chronic_liver_disease"
  ),
  cr_predictors = "",
  del_t_ch_hr_range = c(6.0, 8.5),
  del_t_aki_hr_range = c(11.75, 27.16),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_9$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_9$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_9)


# ---- multi_model_10 ----
cat("del_t_ch_hr_range = c(5.83, 8.0)\ndel_t_aki_hr_range = c(8.75, 49.66)")
multi_model_10 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "PCs_cardio + Vasopressor + AF + HF + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(5.83, 8.0),
  del_t_aki_hr_range = c(8.75, 49.66),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_10$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_10$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_10)


# ---- multi_model_11 ----
cat("del_t_ch_hr_range = c(5.83, 7.66)\ndel_t_aki_hr_range = c(9.16, 39.16)")
multi_model_11 <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "PCs_cardio + Vasopressor + AF + HF + PVD + Chronic_liver_disease"
  ),
  cr_predictors = "cr",
  del_t_ch_hr_range = c(5.83, 7.66),
  del_t_aki_hr_range = c(9.16, 39.16),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(multi_model_11$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
multi_model_11$summary %>%
  mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
  mutate_if(is.integer, as.character) %>%
  t(.) %>%
  data.frame(.) %>%
  kable(., align = c('r'))
rm(multi_model_11)

# "Age + APACHE_II + APACHE_III + Baseline_Cr",
# "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"