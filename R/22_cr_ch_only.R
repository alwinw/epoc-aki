# ---- prev_study_cont ----
cr_ch_prev_study_cont <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = "",
  cr_predictors = c("del_cr"),
  del_t_ch_hr_range = c(3, 4),
  del_t_aki_hr_range = c(8, 16),
  add_gradient_predictor = NULL,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(cr_ch_prev_study_cont$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
summary(cr_ch_prev_study_cont$cutpoint)
rm(cr_ch_prev_study_cont)

# ---- prev_study_bin ----
cr_ch_prev_study_bin <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = "",
  cr_predictors = "",
  del_t_ch_hr_range = c(3, 4),
  del_t_aki_hr_range = c(8, 16),
  add_gradient_predictor = 1,
  all_data = TRUE,
  analysis_data = analysis_df
)
kable(publish(cr_ch_prev_study_bin$model, print = FALSE, digits = c(2, 3))$regressionTable,
      align = c('l', 'c', 'c', 'c', 'c'))
summary(cr_ch_prev_study_bin$cutpoint)
rm(cr_ch_prev_study_bin)

# ---- range_cr_ch_cont_only ----
range_cr_ch_only_cont = tibble(
  del_t_ch_hr_lower = seq(2, 11, by = 1),
  del_t_ch_hr_upper = del_t_ch_hr_lower + 1
) %>%
  mutate(del_t_ch_range = paste0("[", del_t_ch_hr_lower, ", ", del_t_ch_hr_upper, "]")) %>%
  mutate(del_t_ch_range = factor(del_t_ch_range, levels = del_t_ch_range)) %>%
  rowwise() %>%
  do(data.frame(
    .,
    analysis_wrapper(
      outcome_var = "AKI_ICU",
      baseline_predictors = "",
      cr_predictors = "del_cr",
      del_t_ch_hr_range = c(.$del_t_ch_hr_lower, .$del_t_ch_hr_upper),
      del_t_aki_hr_range = c(8, 16),
      add_gradient_predictor = NULL,
      heuristic_only = TRUE,
      analysis_data = analysis_df
    )
  )) %>%
  ungroup() %>%
  pivot_longer(cols = c(AUC, per_admin_in), names_to = "names", values_to = "values") %>%
  mutate(
    labels = if_else(names == "AUC", sprintf("%.2f", values), as.character(n_admissions)),
    names  = if_else(names == "AUC", "AUC", "Admissions"),
    names  = factor(names, levels = c("AUC", "Admissions"))
  )

ggplot(range_cr_ch_only_cont,
       aes(x = del_t_ch_range, y = values, fill = names, colour = names)
  ) +
  geom_col(position = "dodge", alpha = 0.5, colour = NA) +
  geom_label(
    aes(label = labels),
    position = position_dodge(0.9), vjust = -0.2, fill = "white",
    show.legend = FALSE
  ) +
  geom_point(position = position_dodge(0.9)) +
  scale_y_continuous(
    limits = c(0, 0.8),
    breaks = seq(0, 0.8, by = 0.1),
    sec.axis = sec_axis(
      trans = ~.*length(unique(analysis_df$AdmissionID)),
      name = "Number of Included Admissions",
      breaks = round(seq(0, 0.8, by = 0.1)*length(unique(analysis_df$AdmissionID)), 0)
    )
  ) +
  ggtitle("AUC and Number of Admissions for Various \u0394t Increments") +
  xlab(expression("Duration of small change in Cr epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
  ylab("AUC") +
  scale_fill_manual(name = "Legend", values = c("orange","blue", "black")) +
  scale_colour_manual(name = "Legend", values = c("orange","blue", "black")) +
  theme(legend.position = "bottom")
rm(range_cr_ch_only_cont)

# ---- range_cr_ch_bin_only ----
range_cr_ch_only_bin = tibble(
  del_t_ch_hr_lower = seq(2, 11, by = 1),
  del_t_ch_hr_upper = del_t_ch_hr_lower + 1
) %>%
  mutate(del_t_ch_range = paste0("[", del_t_ch_hr_lower, ", ", del_t_ch_hr_upper, "]")) %>%
  mutate(del_t_ch_range = factor(del_t_ch_range, levels = del_t_ch_range)) %>%
  rowwise() %>%
  do(data.frame(
    .,
    analysis_wrapper(
      outcome_var = "AKI_ICU",
      baseline_predictors = "",
      cr_predictors = "",
      del_t_ch_hr_range = c(.$del_t_ch_hr_lower, .$del_t_ch_hr_upper),
      del_t_aki_hr_range = c(8, 16),
      add_gradient_predictor = 1,
      heuristic_only = TRUE,
      analysis_data = analysis_df
    )
  )) %>%
  ungroup() %>%
  pivot_longer(cols = c(AUC, per_admin_in), names_to = "names", values_to = "values") %>%
  mutate(
    labels = if_else(names == "AUC", sprintf("%.2f", values), as.character(n_admissions)),
    names  = if_else(names == "AUC", "AUC", "Admissions"),
    names  = factor(names, levels = c("AUC", "Admissions"))
  )

ggplot(range_cr_ch_only_bin,
       aes(x = del_t_ch_range, y = values, fill = names, colour = names)
) +
  geom_col(position = "dodge", alpha = 0.5, colour = NA) +
  geom_label(
    aes(label = labels),
    position = position_dodge(0.9), vjust = -0.2, fill = "white",
    show.legend = FALSE
  ) +
  geom_point(position = position_dodge(0.9)) +
  scale_y_continuous(
    limits = c(0, 0.8),
    breaks = seq(0, 0.8, by = 0.1),
    sec.axis = sec_axis(
      trans = ~.*length(unique(analysis_df$AdmissionID)),
      name = "Number of Included Admissions",
      breaks = round(seq(0, 0.8, by = 0.1)*length(unique(analysis_df$AdmissionID)), 0)
    )
  ) +
  ggtitle("AUC and Number of Admissions for Various \u0394t Increments") +
  xlab(expression("Duration of small change in Cr epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
  ylab("AUC") +
  scale_fill_manual(name = "Legend", values = c("orange","blue", "black")) +
  scale_colour_manual(name = "Legend", values = c("orange","blue", "black")) +
  theme(legend.position = "bottom")
rm(range_cr_ch_only_bin)
