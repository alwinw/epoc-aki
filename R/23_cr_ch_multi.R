# ---- generate_example_cont_fun ----
generate_example <- function(
  crch_centre,
  t_interval_width,
  min_hr_until_aki,
  max_hr_until_aki,
  add_gradient_predictor = NULL
) {
  lower_crch = crch_centre - t_interval_width/2
  upper_crch = crch_centre + t_interval_width/2

  result <- analysis_wrapper(
    outcome_var = "AKI_ICU",
    baseline_predictors = c(
      "Age + APACHE_II + APACHE_III + Baseline_Cr",
      "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
    ),
    cr_predictors = c("del_cr", "cr"),
    del_t_ch_hr_range = c(lower_crch, upper_crch),
    del_t_aki_hr_range = c(min_hr_until_aki, max_hr_until_aki),
    add_gradient_predictor = add_gradient_predictor,
    all_data = TRUE,
    analysis_data = analysis_df
  )

  heatmap_factors = factor(
    c("No AKI", paste0("T_AKI in ", min_hr_until_aki, "-", max_hr_until_aki, "hrs after cr_ch")
  ))

  result_df <- result$summary %>%
    pivot_longer(
      ends_with("pos")|ends_with("neg"),
      names_to = c("admission", "heatmap"), values_to = "count",
      names_pattern = "n_?(.*)_(.*)"
    ) %>%
    pivot_wider(names_from = admission, values_from = count) %>%
    mutate(heatmap = heatmap_factors[if_else(heatmap == "neg",1,2)])

  if (!is.null(add_gradient_predictor)) {
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
  result_confusion_matrix = summary(result$cutpoint)$confusion_matrix

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
    annotate("tile", x = crch_centre, y = 2.5, width = t_interval_width, height = 55,
             fill = "white", colour = NA, alpha = 0.1, size = 0.2
    ) +
    geom_abline(slope = 1, intercept = 0, colour = "white", linetype = "dotted") +
    annotate("text", x = 10, y = 11, label = "1\u03BCmol/L/h", colour = "white", vjust = -0.3) +
  # if (!is.null(add_gradient_predictor)) {
    # heatmap_plot <- heatmap_plot +
      geom_path(aes(x = x, y = y), data = binary_path, colour = "white") +
  # }
    geom_path(aes(x = x, y = y), data = binary_path, colour = "white") +
    geom_text(
      aes(x = crch_centre, y = 22,
          label = paste0("Included Cr_ch: ", lower_crch, " < \u0394t < ", upper_crch,
                         "\nn(Admissions): ", admissions,
                         "\nn(Cr_ch epis): ", event)),
      data = result_df,
      colour = "white", hjust = 0.5, vjust = -0.1,
    ) +
    geom_text(
      aes(x = 0.2, y = -24,label = paste0("All Cr_ch:\nn(Admissions): ", n_admission, "\nn(Cr_ch epis): ", n_cr)),
      data = heatmap_count,
      colour = "white", hjust = 0, vjust = 0
    ) +
    facet_wrap(~heatmap, nrow = 1) +
    scale_x_continuous(breaks = seq(0, 12, by = 2)) +
    coord_cartesian(xlim = c(0, 12), ylim = c(-25, 30), expand = FALSE) +
    ggtitle(paste0(
      "Creatinine Changes in time interval ", lower_crch, " < \u0394t < ", upper_crch,
      " yields AUC: ", round(result_df$AUC[1], 4))) +
    xlab(expression("Duration of small change in Cr epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
    ylab(expression("Change in Cr during epis: "*Delta*"cr"*" ("*mu*"mol/L)")) +
    scale_fill_viridis_c("n(Cr_ch epis)\nDensity", option = "D") +
    theme(panel.spacing = unit(0.8, "lines"))
  print(heatmap_plot)

  return(result)
}






# ---- generate_example_cont_fun ----
generate_example <- function(crch_centre, t_interval_width, min_hr_until_aki, max_hr_until_aki) {
  result <- cr_ch_cont_model(
    c(crch_centre - t_interval_width/2, crch_centre + t_interval_width/2),
    c(min_hr_until_aki, max_hr_until_aki),
    model = TRUE
  )

  result_df <- result$output %>%
    pivot_longer(
      ends_with("pos")|ends_with("neg"),
      names_to = c("admission", "heatmap"), values_to = "count",
      names_pattern = "n_?(.*)_(.*)"
    ) %>%
    pivot_wider(
      names_from = admission,
      values_from = count
    ) %>%
    mutate(
      heatmap = if_else(
        heatmap == "neg",
        "No AKI",
        paste0("T_AKI in  ", min_hr_until_aki, "-", max_hr_until_aki, "hrs after cr_ch"))
    )

  heatmap_all <- logit_df %>%
    filter(is.na(del_t_aki_hr) | del_t_aki_hr > min_hr_until_aki & del_t_aki_hr < max_hr_until_aki) %>%
    mutate(
      heatmap = case_when(  # TODO change to factor and make easier to use
        is.na(del_t_aki_hr)          ~ "No AKI",
        del_t_aki_hr > max_hr_until_aki  ~ paste0("T_AKI in ", max_hr_until_aki, "+hrs after cr_ch"),
        del_t_aki_hr > min_hr_until_aki ~ paste0("T_AKI in  ", min_hr_until_aki, "-", max_hr_until_aki, "hrs after cr_ch"),
        TRUE                      ~ NA_character_
      ),
    )

  heatmap_count <- heatmap_all %>%
    group_by(heatmap) %>%
    summarise(n_cr = n(), n_admission = n_distinct(AdmissionID), .groups = "keep")

  heatmap_ts <- heatmap_all %>%
    filter(del_t_ch_hr < 13, abs(del_cr) < 50) %>%
    select(del_t_ch_hr, del_cr, heatmap)

  heatmap_plot <- ggplot(heatmap_ts, aes(x = del_t_ch_hr, y = del_cr)) +
    geom_density_2d_filled(contour_var = "density") +
    geom_hline(yintercept = 0, colour = "white", linetype = "dotted") +
    annotate("tile", x = crch_centre, y = 2.5, width = t_interval_width, height = 55,
      fill = "white", colour = NA, alpha = 0.1, size = 0.2
    ) +
    annotate(
      "segment",
      x    = c(crch_centre - t_interval_width/2, crch_centre + t_interval_width/2),
      xend = c(crch_centre - t_interval_width/2, crch_centre + t_interval_width/2),
      y = c(-22, -22),
      yend = c(22, 22),
      colour = "white"
    ) +
    geom_vline(xintercept = crch_centre, colour = "white", linetype = "dotted") +
    geom_text(
      aes(x = crch_centre, y = 22,
          label = paste0("Captured cr_ch: ", crch_centre - t_interval_width/2, " < \u0394t < ", crch_centre + t_interval_width/2,
                         "\nn(Admissions): ", admissions,
                         "\nn(Events): ", event)),
      data = result_df,
      colour = "white", hjust = 0.5, vjust = -0.1,
    ) +
    geom_text(
      aes(x = 0.2, y = -24,
          label = paste0("All cr_ch:\nn(Admissions): ", n_admission, "\nn(cr_ch events): ", n_cr)),
      data = heatmap_count,
      colour = "white", hjust = 0, vjust = 0
    ) +
    facet_wrap(~heatmap, nrow = 1) +
    scale_x_continuous(breaks = seq(0, 12, by = 2)) +
    coord_cartesian(xlim = c(0, 12), ylim = c(-25, 30), expand = FALSE) +
    ggtitle(paste0("Creatinine Changes in time interval ",
                   crch_centre - t_interval_width/2, " < \u0394t < ", crch_centre + t_interval_width/2,
                   " yields AUC: ", round(result_df$AUC[1], 4))) +
    xlab(expression("Duration of small change in Cr epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
    ylab(expression("Change in Cr during epis: "*Delta*"cr"*" ("*mu*"mol/L)")) +
    scale_fill_viridis_d("Density") +
    theme(panel.spacing = unit(0.8, "lines"))

  plot(heatmap_plot)

  return(result)
}

# ---- example_cont_1 ----
example_cont_1 <- generate_example(
  crch_centre = 4,
  t_interval_width = 3,
  min_hr_until_aki = 8,
  max_hr_until_aki = 48)
kable(publish(example_cont_1$model, print=FALSE)$regressionTable)
plot(example_cont_1$cutpoint)

# ---- example_cont_2 ----
example_cont_2 <- generate_example(
  crch_centre = 6.5,
  t_interval_width = 1,
  min_hr_until_aki = 8,
  max_hr_until_aki = 16)
kable(publish(example_cont_2$model, print=FALSE)$regressionTable)
plot(example_cont_2$cutpoint)
