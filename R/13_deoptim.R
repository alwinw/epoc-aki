# ---- Heuristic Search Functions ----
tanh_penalty <- function(x, c, d, s) {
  1 / 2 + 1 / 2 * tanh(s / d * atanh(0.8) * (x - c))
}

heuristic_penalty <- function(summary) {
  0 +
    (1 - summary$AUC) * 5 +
    (1 - summary$per_admin_in) * 2 +
    (1 - summary$per_admin_pos) * 3 +
    tanh_penalty(summary$ch_hr_lower, 3.25, 0.25, -1) * 5 +
    tanh_penalty(summary$ch_hr_lower, 9, 1, 1) +
    tanh_penalty(summary$ch_hr_upper, 9, 1, 1) +
    tanh_penalty(summary$aki_hr_upper, 15, 15, -1) +
    tanh_penalty(summary$aki_hr_upper, 60, 10, 1) +
    grepl("\\bAPACHE_II\\b", summary$glm_model) * 5 +
    grepl("\\bAPACHE_III\\b", summary$glm_model) * 5 +
    grepl("\\bBaseline_Cr\\b", summary$glm_model) +
    grepl("\\bcr\\b", summary$glm_model) * 3 +
    (1 - grepl("cr_gradient", summary$glm_model)) * 6
}

heuristic_wrapper <- function(
                              x,
                              penalty_fn = function(x) x$penalty,
                              return_fn = function(x) x,
                              ...) {
  summary <- aki_dev_wrapper(
    ch_hr_lim = c(x[1] - x[2] / 2, x[1] + x[2] / 2),
    aki_hr_lim = c(x[3], x[3] + x[4]),
    ...
  )
  summary$penalty <- penalty_fn(summary)
  return(return_fn(summary))
}

deoptim_wrapper <- function(
                            lower, upper,
                            itermax,
                            NP = 320,
                            ...,
                            return_fn = function(summary) summary$penalty,
                            fnMap = function(x) round(x, 1),
                            parallel = FALSE) {
  if (parallel) {
    control <- DEoptim.control(
      itermax = itermax,
      NP = NP,
      reltol = 1e-5,
      parallelType = 1,
      packages = c("dplyr", "cutpointr"),
      parVar = c("analysis_df", "aki_dev_wrapper", "heuristic_penalty", "tanh_penalty")
    )
  } else {
    control <- DEoptim.control(
      itermax = itermax,
      NP = NP,
      reltol = 1e-5
    )
  }

  result <- DEoptim(
    heuristic_wrapper,
    lower = lower,
    upper = upper,
    control = control,
    ..., # Passed into the heuristic wrapper
    return_fn = return_fn, # Also passed into the heuristic wrapper
    fnMap = fnMap
  )
  return(list(
    bestmem = heuristic_wrapper(result$optim$bestmem, ...),
    result = result
  ))
}

# Example:
# example1 <- deoptim_wrapper(
#   lower = c(4, 0.5, 3, 1),
#   upper = c(6, 6, 12, 72),
#   itermax = 3, # 20 brief test
#   NP = 32,
#   analysis_data = epoc_aki$analysis,
#   outcome_var = "AKI_2or3",
#   baseline_predictors = NULL,
#   cr_predictors = NULL,
#   add_gradient_predictor = 1,
#   penalty_fn = heuristic_penalty,
#   parallel = FALSE
# )
# example2 <- deoptim_wrapper(
#   lower = c(4, 0.5, 3, 1),
#   upper = c(6, 6, 12, 72),
#   itermax = 3, # 20 brief test
#   NP = 32,
#   outcome_var = "AKI_2or3",
#   baseline_predictors = NULL,
#   cr_predictors = NULL,
#   add_gradient_predictor = 1,
#   penalty_fn = heuristic_penalty,
#   parallel = TRUE
# )


# ---- deoptim functions ----
deoptim_search <- function(
                           # aki_dev_wrapper
                           analysis_data,
                           outcome_var,
                           baseline_predictors,
                           cr_predictors,
                           add_gradient_predictor,
                           first_cr_only,
                           stepwise = FALSE,
                           k = "mBIC",
                           # de_optim
                           penalty_fn = heuristic_penalty,
                           itermax = 200,
                           NP = 320,
                           parallel = TRUE,
                           # extra
                           secondary_outcomes = c(
                             "AKI_ICU",
                             "Cr_defined_AKI_2or3", "Cr_defined_AKI",
                             "Olig_defined_AKI_2or3", "Olig_defined_AKI"
                           ),
                           override = NULL) {
  if (is.null(override)) {
    optim_value <- deoptim_wrapper(
      lower = c(4, 0.5, 3, 1),
      upper = c(6, 6, 12, 72),
      analysis_data = analysis_data,
      outcome_var = outcome_var,
      baseline_predictors = baseline_predictors,
      cr_predictors = cr_predictors,
      add_gradient_predictor = add_gradient_predictor,
      stepwise = stepwise,
      k = k,
      penalty_fn = heuristic_penalty,
      itermax = itermax,
      NP = NP,
      parallel = parallel
    )
  } else {
    optim_value <- list(result = list(optim = list(bestmem = override)))
  }

  optim_model <- heuristic_wrapper(
    optim_value$result$optim$bestmem,
    outcome_var = outcome_var,
    baseline_predictors = baseline_predictors,
    cr_predictors = cr_predictors,
    add_gradient_predictor = add_gradient_predictor,
    first_cr_only = first_cr_only,
    stepwise = stepwise,
    k = k,
    all_data = TRUE
  )
  cat("\n----------------\nOptimised model found:\n")
  print(kable(publish(optim_model$model, print = FALSE, digits = c(2, 3))$regressionTable %>% select(-Units), align = c("l", "r", "c", "r")))
  print(kable(t(optim_model$summary), col.names = paste("Outcome:", outcome_var)))

  optim_model_full <- heuristic_wrapper(
    optim_value$result$optim$bestmem,
    outcome_var = outcome_var,
    baseline_predictors = baseline_predictors,
    cr_predictors = cr_predictors,
    add_gradient_predictor = add_gradient_predictor,
    all_data = TRUE,
  )
  cat("\n----------------\nOptimised model with all variables:\n")
  print(kable(publish(optim_model_full$model, print = FALSE, digits = c(2, 3))$regressionTable %>% select(-Units), align = c("l", "r", "c", "r")))
  print(kable(t(optim_model_full$summary), col.names = paste("Outcome:", outcome_var)))


  if (!is.null(baseline_predictors)) {
    baseline_all <- aki_dev_wrapper( # Must be with baseline_df
      outcome_var = outcome_var,
      baseline_predictors = baseline_predictors,
      cr_predictors = NULL,
      add_gradient_predictor = NULL,
      all_data = TRUE,
      analysis_data = baseline_df
    )
    cat("\n----------------\nBaseline model for all admissions:\n")
    print(kable(publish(baseline_all$model, print = FALSE, digits = c(2, 3))$regressionTable %>% select(-Units), align = c("l", "r", "c", "r")))
    print(kable(t(baseline_all$summary), col.names = paste("Outcome:", outcome_var)))

    baseline_sig <- aki_dev_wrapper( # Must be with baseline_df
      outcome_var = outcome_var,
      baseline_predictors = baseline_predictors,
      cr_predictors = NULL,
      add_gradient_predictor = NULL,
      stepwise = TRUE,
      k = "AIC",
      all_data = TRUE,
      analysis_data = baseline_df
    )
    cat("\n----------------\nBaseline model for all admissions (sig only):\n")
    print(kable(publish(baseline_sig$model, print = FALSE, digits = c(2, 3))$regressionTable %>% select(-Units), align = c("l", "r", "c", "r")))
    print(kable(t(baseline_sig$summary), col.names = paste("Outcome:", outcome_var)))
  }

  # Update the predictors if it was stepwise!
  baseline_predictors <- gsub(".*~ | \\+ \\bcr\\b| \\+ \\bcr_gradient\\b", "", optim_model$summary$glm_model)
  if ((baseline_predictors) %in% c("cr", "cr_gradient")) baseline_predictors <- NULL
  if (!grepl("\\bcr\\b", optim_model$summary$glm_model)) cr_predictors <- NULL
  if (!grepl("\\bcr_gradient\\b", optim_model$summary$glm_model)) add_gradient_predictor <- NULL

  secondary_models <- lapply(
    secondary_outcomes,
    function(outcome_var) {
      secondary_model <- heuristic_wrapper(
        optim_value$result$optim$bestmem,
        outcome_var = outcome_var,
        baseline_predictors = baseline_predictors,
        cr_predictors = cr_predictors,
        add_gradient_predictor = add_gradient_predictor,
        all_data = TRUE
      )
      cat(paste0("\n----------------\nSame model with secondary outcome ", outcome_var, ":\n"))
      print(kable(publish(secondary_model$model, print = FALSE, digits = c(2, 3))$regressionTable %>% select(-Units), align = c("l", "r", "c", "r")))
      print(kable(t(secondary_model$summary), col.names = paste("Outcome:", outcome_var)))
      return(secondary_model)
    }
  )
  names(secondary_models) <- secondary_outcomes

  # CR ONLY models

  if (is.null(baseline_predictors)) {
    return(list(
      optim_value = optim_value,
      optim_model = optim_model,
      optim_model_full = optim_model_full,
      secondary_models = secondary_models
    ))
  } else {
    return(list(
      optim_value = optim_value,
      optim_model = optim_model,
      optim_model_full = optim_model_full,
      baseline_models = list(baseline_all = baseline_all, baseline_sig = baseline_sig),
      secondary_models = secondary_models
    ))
  }
}

# Cr gradient only model
grad_only_model <- deoptim_search(
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = NULL,
  add_gradient_predictor = 1,
  stepwise = FALSE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  ),
  override = c(5.7, 3.2, 3.0, 34.7)
)

# Cr change model
change_only_model <- deoptim_search(
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = "del_cr",
  add_gradient_predictor = NULL,
  stepwise = FALSE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  ),
  override = c(5.6, 3.1, 3.0, 34.7)
)

# Cr percentage change model
per_only_model <- deoptim_search(
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = "per_cr_change",
  add_gradient_predictor = NULL,
  stepwise = FALSE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  ),
  override = c(5.6, 3.1, 3.0, 34.7)
)

# Multivariate model
multi_model <- deoptim_search(
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + PVD + Chronic_liver_disease" # HT excluded
  ),
  cr_predictors = "cr", # c("cr", "per_cr_change"), # Put this into an "alternate" model
  add_gradient_predictor = 1,
  first_cr_only = FALSE,
  stepwise = TRUE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  ),
  override = c(4.9, 1.8, 8.7, 16.9)
)
