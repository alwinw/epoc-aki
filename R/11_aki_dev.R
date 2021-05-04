# Filter for given cr and aki
# Generic function

# ---- Create Outcomes ----
create_outcomes <- function(outcome_var,
                            ch_hr_lim,
                            aki_hr_lim,
                            analysis_data) {
  ch_hr_lim <- sort(ch_hr_lim)
  aki_hr_lim <- sort(aki_hr_lim)
  analysis_data %>%
    filter(del_t_ch_hr >= ch_hr_lim[1], del_t_ch_hr <= ch_hr_lim[2]) %>%
    # Lookahead
    mutate(
      {{ outcome_var }} := case_when(
        is.na(del_t_aki_hr) ~ 0,
        del_t_aki_hr < aki_hr_lim[1] ~ .data[[outcome_var]],
        del_t_aki_hr >= aki_hr_lim[1] & del_t_aki_hr <= aki_hr_lim[2] ~ .data[[outcome_var]],
        del_t_aki_hr > aki_hr_lim[2] ~ 0,
        TRUE ~ NA_real_
      )
    )
}

if (FALSE) {
  create_outcomes(
    outcome_var = "AKI_2or3",
    ch_hr_lim = c(5, 6),
    aki_hr_lim = c(12, 24),
    analysis_data =
      tribble(
        ~del_t_ch_hr, ~del_t_aki_hr, ~AKI_2or3,
        4, 3, 0, # should be filtered out
        5, 6, 0, # should be 0
        5, 6, 1, # should be 1
        5, NA, 0, # should be 0
        5, 15, 0, # should be 0
        5, 15, 1, # should be 1
        5, NA, 0, # should be 0
        5, 30, 0, # should be 0
        5, 30, 1 # should be 0
      )
  )
}

# ---- aki_dev_wrapper ----
aki_dev_wrapper <- function(analysis_data,
                            outcome_var,
                            baseline_predictors,
                            cr_predictors,
                            add_gradient_predictor,
                            ch_hr_lim,
                            aki_hr_lim,
                            first_cr_only = FALSE,
                            stepwise = FALSE,
                            k = "mBIC",
                            all_data = FALSE
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
  ch_hr_lim <- sort(ch_hr_lim)
  analysis_data <- filter(analysis_data, del_t_ch_hr >= ch_hr_lim[1], del_t_ch_hr <= ch_hr_lim[2])
  summary$ch_hr_lower <- ch_hr_lim[1]
  summary$ch_hr_upper <- ch_hr_lim[2]
  # Apply aki_hr filter
  aki_hr_lim <- sort(aki_hr_lim)
  analysis_data <- filter(analysis_data, is.na(del_t_aki_hr) | del_t_aki_hr >= aki_hr_lim[1] & del_t_aki_hr <= aki_hr_lim[2])
  summary$aki_hr_lower <- aki_hr_lim[1]
  summary$aki_hr_upper <- aki_hr_lim[2]
  # Remove any very large jumps
  analysis_data <- filter(analysis_data, abs(del_cr) < 100) # Consider if this is reasonable or not
  # Apply first cr_change only
  if (first_cr_only & "DateTime_Pathology_Result" %in% colnames(analysis_data)) {
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
    predictors <- c(predictors, "cr_gradient") # TODO should be a paste0("cr_gradient", add_gradient_predictor)
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
  summary$n_UR <- length(unique(analysis_data$UR_number))
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
# analysis_df = epoc_aki$analysis
#
# test1func <- aki_dev_wrapper(
#   analysis_data = analysis_df,
#   outcome_var = "AKI_ICU",
#   baseline_predictors = c(
#     "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
#     "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
#   ),
#   cr_predictors = "cr",
#   add_gradient_predictor = 1, # umol/L/h
#   ch_hr_lim = c(-Inf, Inf),
#   aki_hr_lim = c(-Inf, Inf)
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
#   ch_hr_lim = c(6, 7),
#   aki_hr_lim = c(8, 48)
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
#   ch_hr_lim = c(6, 7),
#   aki_hr_lim = c(8, 48),
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
#   ch_hr_lim = c(6.0, 8.33),
#   aki_hr_lim = c(9.25, 47.33),
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

print_model_summary <- function(model, print = TRUE) {
  # TODO add option to print custom message at the top
  if (!print) {
    return(NULL)
  }
  print(kable(
    publish(model$model, print = FALSE, digits = c(2, 3))$regressionTable %>%
      select(-Units),
    align = c("l", "r", "c", "r")
  ))
  print(kable(
    t(model$summary),
    col.names = paste("Outcome:", gsub(" ~.*", "", model$summary$glm_model))
  ))
}

if (FALSE) {
  test_df <- create_outcomes(
    outcome_var = "AKI_2or3",
    ch_hr_lim = c(4, 5.8),
    aki_hr_lim = c(8.7, 25.6),
    analysis_data = analysis_df
  )

  temp <- lmer(AKI_2or3 ~ APACHE_II + APACHE_III + PCs_cardio + Vasopressor + Chronic_liver_disease + (1 | AdmissionID), test_df)
  summary(temp)
  test_df$predict <- predict(temp, type = "response")
  logit_cut <- cutpointr(
    test_df, predict, {{ outcome_var }},
    use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
    method = maximize_metric, metric = youden
  )
  summary(logit_cut)

  temp2 <- glm(formula = AKI_2or3 ~ APACHE_II + APACHE_III + PCs_cardio + Vasopressor + Chronic_liver_disease, family = "binomial", data = analysis_data)
  summary(temp2)
}
