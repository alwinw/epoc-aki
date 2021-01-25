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
                            del_t_ch_hr_range = NULL,
                            del_t_aki_hr_range = NULL,
                            add_gradient_predictor = NULL,
                            stepwise = FALSE,
                            k = "mBIC",
                            plot_cutpoint = FALSE,
                            heuristic_only = FALSE,
                            all_data = FALSE,
                            analysis_data = analysis_df) {
  # Defaults
  glm_model <- paste(outcome_var, "~", paste(baseline_predictors, collapse = " + "))
  n_analysis_data <- length(unique(analysis_data$AdmissionID))

  # Apply any filters
  if (!is.null(del_t_ch_hr_range)) {
    del_t_ch_hr_range <- sort(del_t_ch_hr_range)
    if (del_t_ch_hr_range[1] < 0) {
      warning(paste0("Lower del_t_ch should be > 0, not '", del_t_ch_hr_range[1], "'"))
      return(NULL)
    }
    analysis_data <- filter(
      analysis_data,
      del_t_ch_hr >= del_t_ch_hr_range[1],
      del_t_ch_hr <= del_t_ch_hr_range[2]
    )
  }
  if (!is.null(del_t_aki_hr_range)) {
    del_t_aki_hr_range <- sort(del_t_aki_hr_range)
    if (del_t_ch_hr_range[1] < 0) {
      warning(paste0("Lower del_t_aki should be > 0, not '", del_t_aki_hr_range[1], "'"))
      return(NULL)
    }
    analysis_data <- filter(
      analysis_data,
      is.na(del_t_aki_hr) |
        del_t_aki_hr >= del_t_aki_hr_range[1] &
          del_t_aki_hr <= del_t_aki_hr_range[2]
    )
  }
  if (nrow(analysis_data) == 0) {
    warning(paste0("No rows in analysis_data found"))
    return(NULL)
  }

  # Add cr variables
  if (!is.null(cr_predictors)) {
    glm_model <- paste(glm_model, paste(cr_predictors, collapse = " + "), sep = " + ")
  }
  if (!is.null(add_gradient_predictor)) {
    analysis_data <- mutate(analysis_data, cr_gradient = if_else(del_cr >= add_gradient_predictor * del_t_ch_hr, 1, 0))
    glm_model <- paste(glm_model, "+ cr_gradient")
  }

  # Run glm
  n_admissions <- length(unique(analysis_data$AdmissionID))
  glm_model <- gsub("~  \\+", "~", glm_model)
  logit_model <- glm(formula = glm_model, family = "binomial", data = analysis_data)
  if (stepwise) {
    analysis_data$all_predict <- predict(logit_model, type = "response")
    logit_cut_all <- cutpointr(
      analysis_data, all_predict, {{ outcome_var }},
      use_midpoints = TRUE,
      direction = ">=", pos_class = 1, neg_class = 0,
      method = maximize_metric, metric = youden
    )
    if (k == "AIC") {
      k <- 2
    } else if (k == "BIC") {
      k <- log(nrow(analysis_data))
    } else {
      k <- log(n_admissions)
    }
    logit_model <- step(logit_model, trace = 0, k = k, direction = "backward") # Modified BIC
    glm_model <- gsub("\\s+", " ", paste0(format(formula(logit_model)), collapse = ""))
  }

  analysis_data$predict <- predict(logit_model, type = "response")
  logit_cut = tryCatch(
    {
      cutpointr(
        analysis_data, predict, {{ outcome_var }},
        use_midpoints = TRUE,
        direction = ">=", pos_class = 1, neg_class = 0,
        method = maximize_metric, metric = youden
      )
    },
    error = function(e) {
      # print(e)
      # warning(e)
      list(AUC = 0, sensitivity = list(0), specificity = list(0))
    }
  )
  if (plot_cutpoint) {
    print(plot(logit_cut))
  }

  # Summary
  if (heuristic_only) {
    return(data.frame(
      AUC = logit_cut$AUC,
      per_admin_in = n_admissions / n_analysis_data,
      n_admissions = n_admissions
    ))
  }
  summary <- data.frame(
    AUC = logit_cut$AUC,
    sensitivity = logit_cut$sensitivity[[1]],
    specificity = logit_cut$specificity[[1]],
    optimal_cutpoint = logit_cut$optimal_cutpoint,
    per_admin_in = n_admissions / n_analysis_data,
    n_admissions = n_admissions,
    n_admissions_pos = length(unique(analysis_data$AdmissionID[analysis_data$AKI_ICU == 1])),
    n_admissions_neg = length(unique(analysis_data$AdmissionID[analysis_data$AKI_ICU == 0])),
    n_UR = length(unique(analysis_data$`UR number`)),
    n = nrow(analysis_data),
    n_event_pos = sum(analysis_data$AKI_ICU == 1),
    n_event_neg = sum(analysis_data$AKI_ICU == 0),
    glm_model = glm_model,
    # Does not handle case where no ranges applied
    ch_hr_lower = del_t_ch_hr_range[1],
    ch_hr_upper = del_t_ch_hr_range[2],
    aki_hr_lower = del_t_aki_hr_range[1],
    aki_hr_upper = del_t_aki_hr_range[2]
  )
  if (stepwise) summary$AUC_all <- logit_cut_all$AUC

  if (!all_data) {
    return(summary)
  } else {
    params <- data.frame(
      glm_model = glm_model,
      ch_hr_lower = NA, ch_hr_upper = NA,
      aki_hr_lower = NA, aki_hr_upper = NA
    )
    if (!is.null(del_t_ch_hr_range)) {
      params$ch_hr_lower <- del_t_ch_hr_range[1]
      params$ch_hr_upper <- del_t_ch_hr_range[2]
    }
    if (!is.null(del_t_aki_hr_range)) {
      params$aki_hr_lower <- del_t_aki_hr_range[1]
      params$aki_hr_upper <- del_t_aki_hr_range[2]
    }
    return(list(
      model = logit_model,
      cutpoint = logit_cut,
      summary = summary,
      data = analysis_data,
      params = params
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
