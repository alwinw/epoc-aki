# ---- generate_example_cont_fun ----
generate_example <- function(crch_centre, t_interval_width, min_hr_until_aki, max_hr_until_aki) {
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
    all_data = TRUE,
    analysis_data = analysis_df
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
    summarise(n_cr = n(), n_admission = n_distinct(AdmissionID), .groups = "drop")
  # heatmap_outcome <- heatmap_all %>%
  #   group_by(heatmap, AKI_ICU) %>%
  #   summarise(n_cr = n(), n_admission = n_distinct(AdmissionID), .groups = "drop")
  heatmap_ts <- heatmap_all %>%
    filter(del_t_ch_hr < 13, abs(del_cr) < 50) %>%
    select(del_t_ch_hr, del_cr, heatmap)
  heatmap_path <- data.frame(
    heatmap = c(rep("No AKI", 4), rep(paste0("T_AKI in  ", min_hr_until_aki, "-", max_hr_until_aki, "hrs after cr_ch"), 4)),
    x = rep(c(rep(lower_crch, 2), rep(upper_crch, 2)), 2),
    y = c(-22, lower_crch*binary_mapping, upper_crch*binary_mapping, -22,
          22, lower_crch*binary_mapping, upper_crch*binary_mapping,  22)
  )
  
  return(result)
}











# ---- baseline_df ----
logit_df <- admission_ts %>%
  select(
    `UR number`:Admission, Pt_Study_nos, Event,
    Age, APACHE_II, APACHE_III, Baseline_Cr, PCs_cardio, Vasopressor:Chronic_liver_disease,
    AKI_ICU,
    del_t_ch:cr_i
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
  ungroup() %>%
  filter(abs(del_cr) < 50) %>%  # Consider if this is reasonable or not
  mutate(
    del_t_ch_hr  = as.numeric(del_t_ch, "hours"),
    del_t_aki_hr = as.numeric(del_t_aki, "hours"))

# ---- cr_ch_function ----
cr_ch_cont_model <- function(vec_del_t_ch_hr, vec_del_t_aki_hr, plot = FALSE, model = FALSE) {
  vec_del_t_ch_hr  = sort(vec_del_t_ch_hr)
  vec_del_t_aki_hr = sort(vec_del_t_aki_hr)
  logit_ts <- logit_df %>%
    filter(
      del_t_ch_hr >= vec_del_t_ch_hr[1],
      del_t_ch_hr <= vec_del_t_ch_hr[2],
      is.na(del_t_aki) |
        del_t_aki_hr >= vec_del_t_aki_hr[1] &
        del_t_aki_hr <= vec_del_t_aki_hr[2]
    )
  if(nrow(logit_ts) == 0 | vec_del_t_ch_hr[1] < 0 | vec_del_t_aki_hr[1] < 0){
    return(data.frame(
      AUC              = 0,
      sensitivity      = 0,
      specificity      = 0,
      optimal_cutpoint = 0,
      per_admin_in     = 0,
      n_admissions     = 0,
      n_admissions_pos = 0,
      n_admissions_neg = 0,
      n_UR             = 0,
      n                = 0,
      n_event_pos      = 0,
      n_event_neg      = 0
    ))
  }

  logit_model <- glm(
    AKI_ICU ~ Age + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio +
      Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease +
      del_cr + cr_i,
    family = "binomial",
    data = logit_ts)

  logit_ts$model = predict(logit_model, type = "response")
  logit_cut <- cutpointr(
    logit_ts, model, AKI_ICU,
    use_midpoints = TRUE,
    direction = ">=", pos_class = 1, neg_class = 0,
    method = maximize_metric, metric = youden)

  if (plot) {
    print(summary(logit_model))
    print(summary(logit_cut))
    print(plot(logit_cut))
  }

  per_admin_in = length(unique(logit_ts$AdmissionID))/length(unique(logit_df$AdmissionID))

  # TODO consider using summarise to generate pos/neg breakdown

  output = data.frame(
    AUC              = logit_cut$AUC,
    sensitivity      = logit_cut$sensitivity[[1]],
    specificity      = logit_cut$specificity[[1]],
    optimal_cutpoint = logit_cut$optimal_cutpoint,
    per_admin_in     = per_admin_in,
    n_admissions     = length(unique(logit_ts$AdmissionID)),
    n_admissions_pos = length(unique(logit_ts$AdmissionID[logit_ts$AKI_ICU == 1])),
    n_admissions_neg = length(unique(logit_ts$AdmissionID[logit_ts$AKI_ICU == 0])),
    n_UR             = length(unique(logit_ts$`UR number`)),
    n                = nrow(logit_ts),
    n_event_pos      = sum(logit_ts$AKI_ICU == 1),
    n_event_neg      = sum(logit_ts$AKI_ICU == 0)
  )
  if (model) {
    return(list(model = logit_model, cutpoint = logit_cut, output = output))
  } else {
    return(output)
  }
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
