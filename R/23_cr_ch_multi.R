# ---- example_cont_1 ----
example_cont_1 <- generate_example(
  crch_centre = 4,
  t_interval_width = 3,
  min_hr_until_aki = 8,
  max_hr_until_aki = 48,
  add_gradient_predictor = NULL
  )
kable(publish(example_cont_1$model, print = FALSE)$regressionTable)
summary(example_cont_1$cutpoint)
plot(example_cont_1$cutpoint)

# ---- example_cont_2 ----
example_cont_2 <- generate_example(
  crch_centre = 6.5,
  t_interval_width = 1,
  min_hr_until_aki = 8,
  max_hr_until_aki = 16)
kable(publish(example_cont_2$model, print = FALSE)$regressionTable)
summary(example_cont_2$cutpoint)
plot(example_cont_2$cutpoint)

# ---- example_bin_2 ----
example_bin_2 <- generate_example(
  crch_centre = 6.5,
  t_interval_width = 1,
  min_hr_until_aki = 8,
  max_hr_until_aki = 16,
  add_gradient_predictor = 1
)
kable(publish(example_bin_2$model, print = FALSE)$regressionTable)
summary(example_bin_2$cutpoint)
plot(example_bin_2$cutpoint)

# ---- range_multi_bin ----
range_cr_ch_multi_bin = tibble(
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
      baseline_predictors = c("Age + APACHE_II + APACHE_III + Baseline_Cr",
      "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"),
      cr_predictors = "cr",
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

ggplot(range_cr_ch_multi_bin,
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
    limits = c(0, 0.9),
    breaks = seq(0, 0.9, by = 0.1),
    sec.axis = sec_axis(
      trans = ~.*length(unique(analysis_df$AdmissionID)),
      name = "Number of Included Admissions",
      breaks = round(seq(0, 0.9, by = 0.1)*length(unique(analysis_df$AdmissionID)), 0)
    )
  ) +
  ggtitle("AUC and Number of Admissions for Various \u0394t Increments") +
  xlab(expression("Duration of small change in Cr epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
  ylab("AUC") +
  scale_fill_manual(name = "Legend", values = c("orange","blue", "black")) +
  scale_colour_manual(name = "Legend", values = c("orange","blue", "black")) +
  theme(legend.position = "bottom")
