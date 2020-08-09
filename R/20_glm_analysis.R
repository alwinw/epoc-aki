# ---- analysis_ts ----
analysis_df <- cr_ch_ts %>%
  select(
    `UR number`:Admission, Pt_Study_nos, Event,
    Age, APACHE_II, APACHE_III, Baseline_Cr, PCs_cardio, Vasopressor:Chronic_liver_disease,
    AKI_ICU, DateTime_Pathology_Result,
    del_t_ch:cr
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
    del_t_aki_hr = as.numeric(del_t_aki, "hours")
  ) %>%
  select(-del_t_ch, -del_t_aki)


# ---- glm_analysis_wrapper ----
analysis_wrapper <- function(
  outcome_var = "AKI_ICU",
  baseline_predictors = c("Age"),
  cr_predictors = NULL,
  del_t_ch_hr_range  = NULL,
  del_t_aki_hr_range = NULL,
  add_gradient_predictor = NULL,
  plot_cutpoint = FALSE,
  heuristic_only = FALSE,
  all_data = FALSE,
  analysis_data = analysis_df
) {
  # Defaults
  glm_model = paste(outcome_var, "~", paste(baseline_predictors, collapse = " + "))
  n_analysis_data = length(unique(analysis_data$AdmissionID))

  if (heuristic_only) {
    null_return = data.frame(AUC = 0, per_admin_in = 0, n_admissions = 0)
  } else {
    null_return = data.frame(
      AUC = 0, sensitivity = 0, specificity = 0, optimal_cutpoint = 0,
      per_admin_in = 0, n_admissions = 0, n_admissions_pos = 0, n_admissions_neg = 0,
      n_UR = 0, n_event = 0, n_event_pos = 0, n_event_neg = 0
    )
  }

  # Apply any filters
  if (!is.null(del_t_ch_hr_range)) {
    del_t_ch_hr_range  = sort(del_t_ch_hr_range)
    if (del_t_ch_hr_range[1] < 0) {
      warning(paste0("Lower del_t_ch should be > 0, not '", del_t_ch_hr_range[1], "'"))
      return(null_return)
    }
    analysis_data <- analysis_data %>%
      filter(del_t_ch_hr >= del_t_ch_hr_range[1],
             del_t_ch_hr <= del_t_ch_hr_range[2])
  }

  if (!is.null(del_t_aki_hr_range)) {
    del_t_aki_hr_range = sort(del_t_aki_hr_range)
    if (del_t_ch_hr_range[1] < 0) {
      warning(paste0("Lower del_t_aki should be > 0, not '", del_t_aki_hr_range[1], "'"))
      return(null_return)
    }
    analysis_data <- analysis_data %>%
      filter(is.na(del_t_aki_hr) |
               del_t_aki_hr >= del_t_aki_hr_range[1] &
               del_t_aki_hr <= del_t_aki_hr_range[2])
  }

  if (nrow(analysis_data) == 0) {
    warning(paste0("No rows in analysis_data found"))
    return(null_return)
  }

  # Add cr variables
  if (!is.null(cr_predictors)) {
    glm_model <- paste(glm_model, paste(cr_predictors, collapse = " + "), sep = " + ")
  }

  # Add binary classification of Cr gradient
  if (!is.null(add_gradient_predictor)) {
    analysis_data <- analysis_data %>%
      mutate(cr_gradient = if_else(del_cr >= add_gradient_predictor*del_t_ch_hr, 1, 0))
    glm_model <- paste(glm_model, "+ cr_gradient")
  }

  # Run glm
  logit_model <- glm(formula = glm_model, family = "binomial", data = analysis_data)

  # Cutponts
  analysis_data$predict = predict(logit_model, type = "response")
  logit_cut <- cutpointr(
    analysis_data,
    predict,
    {{outcome_var}},  # vignette("programming", "dplyr")
    use_midpoints = TRUE,
    direction = ">=", pos_class = 1, neg_class = 0,
    method = maximize_metric, metric = youden)
  if (plot_cutpoint) {
    print(plot(logit_cut))
  }

  # Summary
  per_admin_in = length(unique(analysis_data$AdmissionID))/n_analysis_data

  if (heuristic_only) {
    return(data.frame(
      AUC = logit_cut$AUC, per_admin_in = per_admin_in,
      n_admissions = length(unique(analysis_data$AdmissionID))
    ))
  }
  summary = data.frame(
    AUC              = logit_cut$AUC,
    sensitivity      = logit_cut$sensitivity[[1]],
    specificity      = logit_cut$specificity[[1]],
    optimal_cutpoint = logit_cut$optimal_cutpoint,
    per_admin_in     = per_admin_in,
    n_admissions     = length(unique(analysis_data$AdmissionID)),
    n_admissions_pos = length(unique(analysis_data$AdmissionID[analysis_data$AKI_ICU == 1])),
    n_admissions_neg = length(unique(analysis_data$AdmissionID[analysis_data$AKI_ICU == 0])),
    n_UR             = length(unique(analysis_data$`UR number`)),
    n                = nrow(analysis_data),
    n_event_pos      = sum(analysis_data$AKI_ICU == 1),
    n_event_neg      = sum(analysis_data$AKI_ICU == 0)
  )

  if (!all_data) {
    return(summary)
  } else {
    return(list(model = logit_model, cutpoint = logit_cut, summary = summary, data = analysis_data))
  }
}

# profvis(
#   analysis_wrapper(
#     outcome_var = "AKI_ICU",
#     baseline_predictors = "Age"
#   )
# )


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
    annotate("tile", x = crch_centre, y = 2.5, width = t_interval_width, height = 55,
             fill = "white", colour = NA, alpha = 0.1, size = 0.2
    ) +
    geom_abline(slope = 1, intercept = 0, colour = "white", linetype = "dotted") +
    annotate("text", x = 10, y = 11, label = "1\u03BCmol/L/h", colour = "white", vjust = -0.3) +
    annotate("segment", x = lower_crch, xend = upper_crch,
             y = lower_crch*add_gradient_predictor, yend = upper_crch*add_gradient_predictor,
             colour = "white"
    ) +
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
