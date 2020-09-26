crch_centre = 6.5
t_interval_width = 1
min_hr_until_aki = 8
max_hr_until_aki = 16
add_gradient_predictor = 1

lower_crch = crch_centre - t_interval_width/2
upper_crch = crch_centre + t_interval_width/2

if (is.null(add_gradient_predictor)) {
  cr_predictors = c("cr", "del_cr")
} else {
  cr_predictors = "cr"
}

result <- analysis_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c(
    "Age + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  ),
  cr_predictors = cr_predictors,
  del_t_ch_hr_range = c(lower_crch, upper_crch),
  del_t_aki_hr_range = c(min_hr_until_aki, max_hr_until_aki),
  add_gradient_predictor = add_gradient_predictor,
  all_data = TRUE,
  analysis_data = analysis_df
)

heatmap_factors = factor(
  c(" No AKI", paste0("AKI in ", min_hr_until_aki, " to ", max_hr_until_aki, " hours post Cr change")
  ))

result_df <- result$summary %>%
  pivot_longer(
    ends_with("pos") | ends_with("neg"),
    names_to = c("admission", "heatmap"), values_to = "count",
    names_pattern = "n_?(.*)_(.*)"
  ) %>%
  pivot_wider(names_from = admission, values_from = count) %>%
  mutate(heatmap = heatmap_factors[if_else(heatmap == "neg",1,2)])

if (!is.null(add_gradient_predictor) & FALSE) {
  # Consider readding this at some stage
  binary_count <- result$data %>%
    mutate(heatmap = heatmap_factors[if_else(is.na(del_t_aki_hr),1,2)]) %>%
    group_by(heatmap, cr_gradient) %>%
    summarise(n_cr = n(), n_admission = n_distinct(AdmissionID), .groups = "drop")
  binary_path <- data.frame(
    heatmap = unlist(list(rep(heatmap_factors[1], 4), rep(heatmap_factors[2], 4))),
    x = rep(c(rep(lower_crch, 2), rep(upper_crch, 2)), 2),
    y = c(-22, lower_crch*add_gradient_predictor, upper_crch*add_gradient_predictor, -22,
          22, lower_crch*add_gradient_predictor, upper_crch*add_gradient_predictor,  22)
  )
}
# S3 method - cutpointr:::summary.cutpointr
# result_confusion_matrix = summary(result$cutpoint)$confusion_matrix
heatmap_all <- analysis_df %>%  # Output has already been filtered to cr range but we want all
  filter(is.na(del_t_aki_hr) | del_t_aki_hr > min_hr_until_aki & del_t_aki_hr < max_hr_until_aki) %>%
  mutate(heatmap = heatmap_factors[if_else(is.na(del_t_aki_hr),1,2)]) %>%
  select(AdmissionID, del_t_ch_hr, del_cr, heatmap)
heatmap_count <- heatmap_all %>%
  group_by(heatmap) %>%
  summarise(n_cr = n(), n_admission = n_distinct(AdmissionID), .groups = "drop")
heatmap_ts <- heatmap_all %>%
  filter(del_t_ch_hr < 13, abs(del_cr) < 50) %>%
  select(del_t_ch_hr, del_cr, heatmap)

heatmap_plot <- ggplot(heatmap_ts, aes(x = del_t_ch_hr, y = del_cr)) +
  geom_density_2d_filled(aes(fill = after_stat(level_mid)), contour_var = "density") +
  geom_hline(yintercept = 0, colour = "white", linetype = "dotted") +
  geom_abline(slope = 1, intercept = 0, colour = "white", linetype = "dotted") +
  annotate("text", x = 7, y = 7.3, label = "1\u03BCmol/L/h", colour = "white", vjust = -0.3, size = 5, angle = 16) +
  geom_text(
    aes(x = 0.2, y = -19,
        label = paste0("Number of patients: ", n_admission, "\nNumber of Cr change episodes: ", n_cr)),
    data = heatmap_count,
    colour = "white", hjust = 0, vjust = 0, size = 5
  ) +
  facet_wrap(~heatmap, nrow = 1) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  coord_cartesian(xlim = c(0, 12), ylim = c(-20, 25), expand = FALSE) +
  xlab(expression("Duration of small change in Cr epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
  ylab(expression("Change in Cr during epis:  "*Delta*"cr"*" ("*mu*"mol/L)")) +
  scale_fill_viridis_c("Number of Cr\nchange epis\n(density)", option = "D") +
  theme(panel.spacing = unit(1, "lines"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 16),
        legend.title = element_text(size = 12))
print(heatmap_plot)

png(bg = "transparent")
ggsave("cr_ch_heatmap_ppt_simple.png", heatmap_plot, path = paste0(rel_path, "/doc/images/"),
       type = "cairo-png", bg = "transparent",
       width = 15, height = 8, scale = 0.8)
