# Summaries
summarise_analysis <- function(analysis_df, measurements_df) {
  # TODO tidy this all up so it returns a dataframe of results
  # maybe use n_disctinct and sum or something
  analysis_df %>%
    select(AdmissionID, DateTime_Pathology_Result) %>%
    distinct() %>%
    nrow() %>%
    print(.)
  analysis_df %>%
    nrow() %>%
    print(.)

  analysis_df %>%
    filter(del_t_aki_hr < 0) %>%
    select(AdmissionID, DateTime_Pathology_Result) %>%
    distinct() %>%
    nrow() %>%
    print(.)
  measurements_df %>%
    filter(cr_before_aki == 0) %>%
    nrow() %>%
    print(.)
  analysis_df %>%
    filter(del_t_aki_hr < 0) %>%
    nrow() %>%
    print(.)

  analysis_df %>%
    filter(is.na(del_t_aki_hr) | del_t_aki_hr >= 0) %>%
    select(AdmissionID, DateTime_Pathology_Result) %>%
    distinct() %>%
    nrow() %>%
    print(.)
  measurements_df %>%
    filter(cr_before_aki == 1) %>%
    nrow(.) - 10 %>% # DIFFERENT because the 10pts were NOT removed due to <2 cr measurements
    print(.)
  analysis_df %>%
    filter(is.na(del_t_aki_hr) | del_t_aki_hr >= 0) %>%
    nrow() %>%
    print(.)
  return(NULL)
}


# ---- Heatmap Plot ----
plot_cr_ch_heatmap <- function(analysis_df, outcome_var) {
  heatmap_var <- case_when(
    outcome_var == "AKI_ICU" ~ "AKI",
    outcome_var == "AKI_2or3" ~ "Stage 2 or 3 AKI",
    TRUE ~ gsub("_", " ", outcome_var)
  )
  heatmap_all <- analysis_df %>%
    filter(is.na(del_t_aki_hr) | del_t_aki_hr >= 0) %>%
    mutate(
      heatmap = case_when(
        is.na(del_t_aki_hr) ~ 0,
        .data[[outcome_var]] == 0 ~ 0,
        .data[[outcome_var]] == 1 & del_t_aki_hr > 28 ~ 8,
        .data[[outcome_var]] == 1 ~ del_t_aki_hr %/% 4 + 1,
        TRUE ~ NA_real_
      ),
      heatmap = factor(
        heatmap,
        levels = 0:8,
        labels = c(
          paste0("No ", heatmap_var),
          paste0(heatmap_var, " in ", (0:6) * 4, "-", (1:7) * 4, "hrs"),
          paste0(heatmap_var, " in 28+hrs")
        ),
        ordered = TRUE
      )
    )

  heatmap_count <- heatmap_all %>%
    group_by(heatmap) %>%
    summarise(n_cr = n(), n_admission = n_distinct(AdmissionID), .groups = "drop") %>%
    mutate(
      label = if_else(row_number() == 1, "", as.character(expression("1" * mu * "mol/L/h"))),
      slope = if_else(row_number() == 1, 0, 1),
      intercept = if_else(row_number() == 1, 100, 0),
    )

  heatmap_plot <- ggplot(
    heatmap_all %>% filter(del_t_ch_hr < 13, abs(del_cr) < 50),
    aes(x = del_t_ch_hr, y = del_cr)
  ) +
    # Contour plot
    geom_density_2d_filled(
      aes(fill = after_stat(level_mid)),
      contour_var = "density"
    ) +
    scale_x_continuous(breaks = seq(0, 12, by = 2)) +
    coord_cartesian(xlim = c(0, 12), ylim = c(-25, 30), expand = FALSE) +
    facet_wrap(~heatmap) +
    scale_fill_viridis_c("Density of\nCr change\nepisodes", option = "D") +
    # Overlay
    geom_hline(yintercept = 0, colour = "white", linetype = "dotted") +
    geom_vline(xintercept = seq(0, 16, by = 4), colour = "white", linetype = "dotted") +
    geom_abline(
      data = heatmap_count,
      aes(slope = slope, intercept = intercept),
      colour = "white", linetype = "solid"
    ) +
    geom_text(
      data = heatmap_count,
      aes(x = 10.5, y = 15.5, label = label),
      colour = "white", vjust = 1.3, angle = 10, size = 3, parse = TRUE
    ) +
    geom_text(
      data = heatmap_count,
      aes(x = 0.2, y = -23, label = paste0("No. of Pts: ", n_admission, "\nNo. of Cr change episodes: ", n_cr)),
      colour = "white", hjust = 0, vjust = 0
    ) +
    # Theme
    ggtitle(paste("The frequency at which small, short-term changes in creatinine predict imminent", heatmap_var)) +
    xlab(expression("Duration of short-term Cr change episode: " * Delta * "t" * " (hours)")) +
    ylab(expression("Change in Cr during episode: " * Delta * "Cr" * " (" * mu * "mol/L)")) +
    theme(panel.spacing = unit(0.85, "lines")) +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = "white"),
      text = element_text(size = 15)
    )
  # print(heatmap_plot)

  return(heatmap_plot)
}
