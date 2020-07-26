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
    null_return = data.frame(AUC = 0, per_admin_in = 0)
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
    x = analysis_data$predict,
    class = analysis_data[[outcome_var]],
    use_midpoints = TRUE,
    direction = ">=", pos_class = 1, neg_class = 0,
    method = maximize_metric, metric = youden)
  if (plot_cutpoint) {
    print(plot(logit_cut))
  }

  # Summary
  per_admin_in = length(unique(analysis_data$AdmissionID))/n_analysis_data

  if (heuristic_only) {
    return(data.frame(AUC = logit_cut$AUC, per_admin_in = per_admin_in))
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
