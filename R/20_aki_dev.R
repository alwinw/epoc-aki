# ---- analysis_ts ----
# consider having col for ABG vs BioChem here and apply a filter
analysis_df <- cr_ch_ts %>%
  select(
    `UR number`:Admission, Pt_Study_nos, Event,
    Age:Chronic_liver_disease,
    AKI_ICU, AKI_stage, DateTime_Pathology_Result,
    del_t_ch:cr
  ) %>%
  mutate(
    APACHE_II = if_else(APACHE_II == 0, NA_real_, APACHE_II),
    APACHE_III = if_else(APACHE_III == 0, NA_real_, APACHE_III)
  ) %>%
  group_by(AKI_ICU) %>%
  mutate(
    APACHE_II = if_else(is.na(APACHE_II), median(APACHE_II, na.rm = TRUE), APACHE_II),
    APACHE_III = if_else(is.na(APACHE_III), median(APACHE_III, na.rm = TRUE), APACHE_III)
  ) %>% # FIXME Replace with REAL data
  ungroup() %>%
  filter(abs(del_cr) < 50) %>% # Consider if this is reasonable or not
  mutate(
    del_t_ch_hr = as.numeric(del_t_ch, "hours"),
    del_t_aki_hr = as.numeric(del_t_aki, "hours")
  ) %>%
  mutate(AKI_2or3 = if_else(AKI_stage >= 2, 1, 0, 0)) %>%
  select(-del_t_ch, -del_t_aki)


# ---- aki_dev_wrapper ----
aki_dev_wrapper <- function(
                            outcome_var = "AKI_ICU",
                            baseline_predictors = c("Age"),
                            cr_predictors = NULL,
                            del_t_ch_hr_range = c(-Inf, Inf),
                            del_t_aki_hr_range = c(-Inf, Inf),
                            add_gradient_predictor = NULL,
                            stepwise = FALSE,
                            k = "mBIC",
                            plot_cutpoint = FALSE,
                            heuristic_only = FALSE,
                            all_data = FALSE,
                            analysis_data = analysis_df) {
  # Defaults
  glm_model <- paste(outcome_var, "~", paste(baseline_predictors, cr_predictors, collapse = " + "))
  n_analysis_data <- length(unique(analysis_data$AdmissionID))

  # Create output summary
  summary <- data.frame(
    AUC = 0, sensitivity = 0, specificity = 0, optimal_cutpoint = 0,
    per_admin_in = 0,n_admissions = 0, n_admissions_pos = 0, n_admissions_neg = 0, n_UR = 0, n = 0,
    n_event_pos = 0, n_event_neg = 0, glm_model = 0, AUC_all = 0,
    ch_hr_lower = -Inf, ch_hr_upper = Inf, aki_hr_lower = -Inf, aki_hr_upper = Inf
  )
  
  # Apply ch_hr filter
  del_t_ch_hr_range <- sort(del_t_ch_hr_range)
  analysis_data <- filter(
    analysis_data,
    del_t_ch_hr >= del_t_ch_hr_range[1],
    del_t_ch_hr <= del_t_ch_hr_range[2]
  )
  summary$ch_hr_lower <- del_t_ch_hr_range[1]
  summary$ch_hr_upper <- del_t_ch_hr_range[2]
  # Apply aki_hr filter
  del_t_aki_hr_range <- sort(del_t_aki_hr_range)
  analysis_data <- filter(
    analysis_data,
    is.na(del_t_aki_hr) |
      del_t_aki_hr >= del_t_aki_hr_range[1] &
        del_t_aki_hr <= del_t_aki_hr_range[2]
  )
  summary$aki_hr_lower <- del_t_aki_hr_range[1]
  summary$aki_hr_upper <- del_t_aki_hr_range[2]
  # Check number of rows
  if (nrow(analysis_data) == 0) {
    warning(paste0("No rows in analysis_data found"))
    return(summary)
  }
  summary$n_admissions <- length(unique(analysis_data$AdmissionID))

  # Create cr_gradient
  if (!is.null(add_gradient_predictor)) {
    analysis_data <- mutate(analysis_data, cr_gradient = if_else(del_cr >= add_gradient_predictor * del_t_ch_hr, 1, 0))
    glm_model <- paste(glm_model, "+ cr_gradient")
  }
  
  # Run glm
  logit_model <- tryCatch({
    glm(formula = glm_model, family = "binomial", data = analysis_data)
  },
  error = function(e) {
    warning("glm for complete model failed")
    warning(e)
    e
  })
  if (inherits(logit_model, "error")) return(summary)
  # Cutpoint
  analysis_data$predict <- predict(logit_model, type = "response")
  logit_cut <- cutpointr(
    analysis_data, predict, {{ outcome_var }},
    use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
    method = maximize_metric, metric = youden
  )
  # Update summary
  summary$AUC_all = logit_cut$AUC
  
  # Run stepwise glm
  if (stepwise) {
    if (k == "AIC") {
      k <- 2
    } else if (k == "BIC") {
      k <- log(nrow(analysis_data))
    } else {
      k <- log(summary$n_admissions) # Modified BIC
    }
    logit_model <- tryCatch({
      step(logit_model, trace = 0, k = k, direction = "backward") # Modified BIC
    },
    error = function(e) {
      warning("Stepwise glm failed")
      warning(e)
      e
    })
    if (inherits(logit_model, "error")) return(summary) # all zero EXCEPT AUC_all
    # Cutpoint
    analysis_data$predict <- predict(logit_model, type = "response")
    logit_cut <- cutpointr(
      analysis_data, predict, {{ outcome_var }},
      use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
      method = maximize_metric, metric = youden
    )
  }
  
  # Update summary
  summary$AUC = logit_cut$AUC
  summary$sensitivity = logit_cut$sensitivity[[1]]
  summary$specificity = logit_cut$specificity[[1]]
  summary$optimal_cutpoint = logit_cut$optimal_cutpoint
  summary$per_admin_in = summary$n_admissions / n_analysis_data
  summary$n_admissions_pos = length(unique(analysis_data$AdmissionID[analysis_data$AKI_ICU == 1]))  # FIXME HARD CODED NEEDS FIXING
  summary$n_admissions_neg = length(unique(analysis_data$AdmissionID[analysis_data$AKI_ICU == 0]))
  summary$n_UR = length(unique(analysis_data$`UR number`))
  summary$n = nrow(analysis_data)
  summary$glm_model <- gsub("~(\\s+\\+){1,}", "~", paste0(format(formula(logit_model)), collapse = ""))

  # Return
  if (!all_data) {
    return(summary)
  } else {
    return(list(
      model = logit_model,
      cutpoint = logit_cut,
      summary = summary,
      data = analysis_data,
    ))
  }
}

# temp <- aki_dev_wrapper(
#   outcome_var = "AKI_ICU",
#   baseline_predictors = c(
#     "Age + APACHE_II + APACHE_III + Baseline_Cr",
#     "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
#   ),
#   cr_predictors = "cr",
#   del_t_ch_hr_range = c(6.0, 8.33),
#   del_t_aki_hr_range = c(9.25, 47.33),
#   add_gradient_predictor = 1,
#   stepwise = TRUE,
#   k = "BIC",
#   all_data = TRUE,
#   analysis_data = analysis_df
# )
# summarise_cutpoint(temp)
# temp$params$glm_model
# publish(temp$model, print = FALSE)$regressionTable


# ---- time_aki_wrapper ----
# analysis to determine model for the time to AKI development,
# rather than AKI development as a binary outcome


# ---- summarise_cutpoint_function ----
summarise_cutpoint <- function(model) {
  model$summary %>%
    select(-ends_with("pos"), -ends_with("neg"), -optimal_cutpoint) %>%
    mutate(per_admin_in = sprintf("%.0f%%", per_admin_in * 100)) %>%
    mutate_if(is.double, function(x) sprintf("%.4f", x)) %>%
    mutate_if(is.integer, as.character) %>%
    t(.) %>%
    data.frame(.) %>%
    rownames_to_column() %>%
    set_names(c("Model Attribute", "Value"))
}
