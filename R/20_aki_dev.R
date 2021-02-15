# ---- analysis_ts ----
# consider having col for ABG vs BioChem here and apply a filter !!IMPORTANT
analysis_df <- cr_ch_ts %>%
  select(
    `UR number`:Admission, Pt_Study_nos, Event,
    Age:Chronic_liver_disease,
    AKI_ICU,
    DateTime_Pathology_Result:AKI_2or3,
    Cr_defined_AKI, Cr_defined_AKI_2or3, Olig_defined_AKI, Olig_defined_AKI_2or3
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
  mutate(
    cr_before_aki = if_else(is.na(del_t_aki_hr) | del_t_aki_hr >= 0, 1, 0)
  ) %>%
  ungroup()

measurements_df <- analysis_df %>%
  mutate(temp = is.na(cr)) %>%
  select(-del_cr, -per_cr_change, -del_t_ch_hr:-del_t_aki_hr) %>%
  unique(.) %>%
  mutate(
    del_t_ch_hr = 0, # consider changing to median or something later
    del_t_aki_hr = 0,
    del_cr = temp
  ) %>%
  group_by(AdmissionID, cr_before_aki) %>%
  mutate(
    n_measurements = n()
  ) %>%
  ungroup() %>%
  select(-temp)

baseline_df <- measurements_df %>%
  select(-DateTime_Pathology_Result:-cr) %>%
  group_by(AdmissionID) %>%
  mutate(n_measurements = if_else(cr_before_aki == 1, n_measurements, NA_integer_)) %>%
  fill(n_measurements, .direction = "updown") %>%
  # mutate(n_measurements = if_else(is.na(n_measurements), as.integer(0), n_measurements)) %>%
  select(-cr_before_aki) %>%
  ungroup() %>%
  unique(.)

# if (anyNA(baseline_df)) stop("There is missing data in baseline_df")
stopifnot(length(unique(baseline_df$AdmissionID)) == nrow(baseline_df))
stopifnot(baseline_df %>% filter(AKI_ICU == 0, Cr_defined_AKI == 1) %>% nrow(.) == 0)

# Compare with admissions data
stopifnot(nrow(filter(baseline_df, AKI_ICU == 1)) >= nrow(filter(admission_data, AKI_ICU == 1)))
stopifnot(nrow(filter(baseline_df, Cr_defined_AKI == 1)) >= nrow(filter(admission_data, Cr_defined_AKI == 1)))
stopifnot(nrow(filter(baseline_df, Olig_defined_AKI == 1)) >= nrow(filter(admission_data, `AKI Dx oliguria` == 1)))

baseline_df <- filter(baseline_df, !del_cr)
analysis_df <- filter(analysis_df, !is.na(del_cr))

# --- time_to_aki_plot ----
# analysis_df %>%
#   filter(AKI_ICU == 1) %>%
#   group_by(AdmissionID) %>%
#   summarise(max_t_aki = max(del_t_aki_hr)) %>%
#   ggplot(., aes(max_t_aki)) +
#   geom_histogram() +
#   xlim(0, 30)
#
# analysis_df %>%
#   group_by(AdmissionID) %>%
#   summarise(min_t_ch = min(del_t_ch_hr), max_t_ch = max(del_t_ch_hr)) %>%
#   ggplot(., aes(max_t_aki)) +
#   geom_histogram() +
#   xlim(0, 30)

# ---- aki_dev_wrapper ----
aki_dev_wrapper <- function(
                            outcome_var = "AKI_ICU",
                            baseline_predictors = c("Age"),
                            cr_predictors = NULL,
                            add_gradient_predictor = NULL,
                            del_t_ch_hr_range = c(-Inf, Inf),
                            del_t_aki_hr_range = c(-Inf, Inf),
                            first_cr_only = TRUE,
                            stepwise = FALSE,
                            k = "mBIC",
                            all_data = FALSE,
                            analysis_data = analysis_df
                            # Consider adding pos and neg class here
) {
  # Defaults
  predictors <- c(baseline_predictors, cr_predictors)
  n_analysis_data <- length(unique(analysis_data$AdmissionID))
  n_analysis_data_pos <- length(unique(analysis_data$AdmissionID[analysis_data[outcome_var] == 1])) ## 1 is hard coded...

  # Create output summary
  summary <- data.frame(
    AUC = 0, sensitivity = 0, specificity = 0, optimal_cutpoint = 0,
    per_admin_in = 0, per_admin_pos = 0, n_admissions = 0, n_admissions_pos = 0, n_admissions_neg = 0, n_UR = 0, n = 0,
    n_event_pos = 0, n_event_neg = 0, glm_model = 0, AUC_all = 0,
    ch_hr_lower = -Inf, ch_hr_upper = Inf, aki_hr_lower = -Inf, aki_hr_upper = Inf
  )

  # Apply ch_hr filter
  del_t_ch_hr_range <- sort(del_t_ch_hr_range)
  analysis_data <- filter(analysis_data, del_t_ch_hr >= del_t_ch_hr_range[1], del_t_ch_hr <= del_t_ch_hr_range[2])
  summary$ch_hr_lower <- del_t_ch_hr_range[1]
  summary$ch_hr_upper <- del_t_ch_hr_range[2]
  # Apply aki_hr filter
  del_t_aki_hr_range <- sort(del_t_aki_hr_range)
  analysis_data <- filter(analysis_data, is.na(del_t_aki_hr) | del_t_aki_hr >= del_t_aki_hr_range[1] & del_t_aki_hr <= del_t_aki_hr_range[2])
  summary$aki_hr_lower <- del_t_aki_hr_range[1]
  summary$aki_hr_upper <- del_t_aki_hr_range[2]
  # Remove any very large jumps
  analysis_data <- filter(analysis_data, abs(del_cr) < 100) # Consider if this is reasonable or not
  # Apply first cr_change only
  if (first_cr_only) {
    analysis_data <- analysis_data %>%
      group_by(AdmissionID) %>%
      slice_max(n = 1, desc(DateTime_Pathology_Result)) %>%
      # TODO investigate why slice_min is soo much slower
      slice_min(n = 1, del_t_ch_hr)
  }
  # Check number of rows
  if (nrow(analysis_data) <= 2) {
    warning(paste0("Insufficient rows in analysis_data"))
    return(summary)
  }
  if (nrow(unique(analysis_data[outcome_var])) != 2) {
    warning("Same outcome in all data")
    return(summary)
  }

  summary$n_admissions <- length(unique(analysis_data$AdmissionID))

  # Create cr_gradient
  if (!is.null(add_gradient_predictor)) {
    analysis_data <- mutate(analysis_data, cr_gradient = if_else(del_cr >= add_gradient_predictor * del_t_ch_hr, 1, 0))
    predictors <- c(predictors, "cr_gradient")
  }

  # Run glm
  glm_model <- paste(outcome_var, "~", paste(predictors[predictors != ""], collapse = " + "))
  logit_model <- glm(formula = glm_model, family = "binomial", data = analysis_data)
  # Cutpoint
  analysis_data$predict <- predict(logit_model, type = "response")
  logit_cut <- cutpointr(
    analysis_data, predict, {{ outcome_var }},
    use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
    method = maximize_metric, metric = youden
  )

  # Update summary
  summary$AUC_all <- logit_cut$AUC

  # Run stepwise glm
  if (stepwise) {
    if (k == "AIC") {
      k <- 2
    } else if (k == "BIC") {
      k <- log(nrow(analysis_data))
    } else if (k == "mBIC") { # Modified BIC
      k <- log(summary$n_admissions)
    } else if (!(is.double(k))) {
      stop("k must be a known IC method or double")
    }
    logit_model <- step(logit_model, trace = 0, k = k, direction = "backward") # Modified BIC
    # Cutpoint
    analysis_data$predict <- predict(logit_model, type = "response")
    logit_cut <- cutpointr(
      analysis_data, predict, {{ outcome_var }},
      use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
      method = maximize_metric, metric = youden
    )
  }

  # Update summary
  summary$AUC <- logit_cut$AUC
  summary$sensitivity <- logit_cut$sensitivity[[1]]
  summary$specificity <- logit_cut$specificity[[1]]
  summary$optimal_cutpoint <- logit_cut$optimal_cutpoint
  summary$per_admin_in <- summary$n_admissions / n_analysis_data
  summary$n_admissions_pos <- length(unique(analysis_data$AdmissionID[analysis_data[outcome_var] == logit_cut$pos_class]))
  summary$n_admissions_neg <- length(unique(analysis_data$AdmissionID[analysis_data[outcome_var] == logit_cut$neg_class]))
  summary$per_admin_pos <- summary$n_admissions_pos / n_analysis_data_pos
  summary$n_UR <- length(unique(analysis_data$`UR number`))
  summary$n <- nrow(analysis_data)
  summary$n_event_pos <- sum(analysis_data[outcome_var] == logit_cut$pos_class)
  summary$n_event_neg <- sum(analysis_data[outcome_var] == logit_cut$neg_class)
  summary$glm_model <- gsub("\\~(\\s+\\+){1,}", "~", paste0(format(formula(logit_model)), collapse = ""))
  summary$glm_model <- gsub("\\s+", " ", summary$glm_model)

  # Return
  if (!all_data) {
    return(summary)
  } else {
    return(list(
      model = logit_model,
      cutpoint = logit_cut,
      summary = summary,
      data = analysis_data
    ))
  }
}


# ---- aki_dev_wrapper_tests ----
# test1func <- aki_dev_wrapper(
#   outcome_var = "AKI_ICU",
#   baseline_predictors = c(
#     "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
#     "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
#   ),
#   cr_predictors = "cr",
#   add_gradient_predictor = 1 # umol/L/h
# )
# test1logit <- glm(
#   AKI_ICU ~ Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease + cr + cr_gradient,
#   family = "binomial",
#   data = analysis_df %>% mutate(cr_gradient = if_else(del_cr >= 1 * del_t_ch_hr, 1, 0))
# )
# test1cut <- cutpointr(predict(test1logit), test1logit$data$AKI_ICU, metric = youden)
# summary(test1cut)
# (test1func)
#
# test2func <- aki_dev_wrapper(
#   outcome_var = "AKI_2or3",
#   baseline_predictors = NULL,
#   add_gradient_predictor = 1,
#   del_t_ch_hr_range = c(6, 7),
#   del_t_aki_hr_range = c(8, 48)
# )
# test2logit <- glm(
#   AKI_ICU ~ cr_gradient,
#   family = "binomial",
#   data = analysis_df %>% mutate(cr_gradient = if_else(del_cr >= 1 * del_t_ch_hr, 1, 0)) %>%
#     filter(del_t_ch_hr >= 6, del_t_ch_hr <= 7, is.na(del_t_aki_hr) | del_t_aki_hr >= 8 & del_t_aki_hr <= 48)
# )
# test2cut <- cutpointr(predict(test2logit), test2logit$data$AKI_2or3, metric = youden)
# summary(test2cut)
# (test2func)
#
# test3func <- aki_dev_wrapper(
#   outcome_var = "AKI_ICU",
#   baseline_predictors = c(
#     "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
#     "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
#   ),
#   cr_predictors = "cr",
#   add_gradient_predictor = 1, # umol/L/h
#   del_t_ch_hr_range = c(6, 7),
#   del_t_aki_hr_range = c(8, 48),
#   stepwise = TRUE,
#   k = "mBIC"
# )
# test3logit <- step(
#   glm(
#     AKI_ICU ~ Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease + cr + cr_gradient,
#     family = "binomial",
#     data = analysis_df %>% mutate(cr_gradient = if_else(del_cr >= 1 * del_t_ch_hr, 1, 0)) %>%
#       filter(del_t_ch_hr >= 6, del_t_ch_hr <= 7, is.na(del_t_aki_hr) | del_t_aki_hr >= 8 & del_t_aki_hr <= 48)
#   ),
#   trace = 0,
#   k = log(test3func$n_admissions),
#   direction = "backward"
# )
# test3cut <- cutpointr(predict(test3logit), test3logit$data$AKI_ICU, metric = youden)
# summary(test3cut)
# (test3func)


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
