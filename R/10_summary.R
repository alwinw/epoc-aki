# Summaries
analysis_df %>%
  select(AdmissionID, DateTime_Pathology_Result) %>%
  distinct() %>%
  nrow()
analysis_df %>%
  nrow()

analysis_df %>%
  filter(del_t_aki_hr < 0) %>%
  select(AdmissionID, DateTime_Pathology_Result) %>%
  distinct() %>%
  nrow()
measurements_df %>%
  filter(cr_before_aki == 0) %>%
  nrow()
analysis_df %>%
  filter(del_t_aki_hr < 0) %>%
  nrow()

analysis_df %>%
  filter(is.na(del_t_aki_hr) | del_t_aki_hr >= 0) %>%
  select(AdmissionID, DateTime_Pathology_Result) %>%
  distinct() %>%
  nrow()
measurements_df %>%
  filter(cr_before_aki == 1) %>%
  nrow() - 10 # DIFFERENT because the 10pts were NOT removed due to <2 cr measurements
analysis_df %>%
  filter(is.na(del_t_aki_hr) | del_t_aki_hr >= 0) %>%
  nrow()

# ---- Heatmap Plot ----
if (FALSE) {
  heatmap_all <- cr_ch_ts %>%
    filter(is.na(del_t_aki_hr) | del_t_aki_hr > 0) %>%
    mutate(
      heatmap = case_when(
        is.na(del_t_aki_hr) ~ " No AKI",
        del_t_aki_hr < 4 ~ "t_AKI in  0-4hrs",
        del_t_aki_hr < 8 ~ "t_AKI in  4-8hrs",
        del_t_aki_hr < 12 ~ "t_AKI in  8-12hrs",
        del_t_aki_hr < 16 ~ "t_AKI in 12-16hrs",
        del_t_aki_hr < 20 ~ "t_AKI in 16-20hrs",
        del_t_aki_hr < 24 ~ "t_AKI in 20-24hrs",
        del_t_aki_hr < 30 ~ "t_AKI in 24-30hrs",
        TRUE ~ "t_AKI in 30+hrs"
      ),
    )

  heatmap_count <- heatmap_all %>%
    group_by(heatmap) %>%
    summarise(n_cr = n(), n_admission = n_distinct(AdmissionID), .groups = "keep")
  heatmap_ts <- heatmap_all %>%
    filter(del_t_ch_hr < 13, abs(del_cr) < 50)
  heatmap_plot <- ggplot(heatmap_ts, aes(x = del_t_ch_hr, y = del_cr)) +
    geom_density_2d_filled(
      aes(fill = after_stat(level_mid)),
      contour_var = "density"
    ) +
    # geom_point() +
    scale_x_continuous(breaks = seq(0, 12, by = 2)) +
    coord_cartesian(xlim = c(0, 12), ylim = c(-25, 30), expand = FALSE) +
    facet_wrap(~heatmap) +
    scale_fill_viridis_c("n(Cr_ch epis)\nDensity", option = "D") +
    geom_hline(yintercept = 0, colour = "white", linetype = "dotted") +
    geom_vline(xintercept = seq(0, 16, by = 4), colour = "white", linetype = "dotted") +
    geom_abline(slope = 1, intercept = 0, colour = "white", linetype = "solid") +
    annotate("text", x = 10, y = 9, label = "1\u03BCmol/L/h", colour = "white", vjust = 1.3) +
    geom_text(
      data = heatmap_count,
      aes(x = 0.2, y = -23, label = paste0("n(Admissions): ", n_admission, "\nn(Cr_ch epis): ", n_cr)),
      colour = "white", hjust = 0, vjust = 0
    ) +
    ggtitle("Number of Creatinine Change Episodes which Predict AKI in X hours' time") +
    xlab(expression("Duration of short-term Cr change epis: " * Delta * "t"["cr_ch"] * " (hours)")) +
    ylab(expression("Change in Cr during epis: " * Delta * "cr" * " (" * mu * "mol/L)")) +
    theme(panel.spacing = unit(0.8, "lines")) +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = "white")
    )
  print(heatmap_plot)

  png(bg = "transparent")

  if (save_plots) {
    ggsave("cr_ch_heatmap_ppt.png", heatmap_plot,
      path = paste0(rel_path, "/doc/images/"),
      type = "cairo-png", bg = "transparent",
      width = 15, height = 14, scale = 0.8
    )

    ggsave("cr_ch_heatmap.png", heatmap_plot,
      path = paste0(rel_path, "/doc/images/"),
      width = 12, height = 11.5, scale = 0.8
    )
  }
}
