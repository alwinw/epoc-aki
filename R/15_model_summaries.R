# Summarise into various tables
# One for cr_gradient, one for percentage change
# Multivariable models for different outcomes
# Add a plot showing cr vs time and predictive value


# Table 2 – Predictive value parameters for creatinine change as an independent predictor of AKI

model_ssAOCI <- function(model) {
  data.frame(
    Predictor = gsub(".*~ ", "", model$summary$glm_model),
    `Cr change episode duration (hours)` = sprintf("%.1f-%.1f", model$summary$ch_hr_lower, model$summary$ch_hr_upper),
    Outcome = gsub(" ~.*", "", model$summary$glm_model),
    `Timeframe to AKI (hours)` = sprintf("%.1f-%.1f", model$summary$aki_hr_lower, model$summary$aki_hr_upper),
    Sensitivity = sprintf("%.0f%%", model$summary$sensitivity * 100),
    Specificity = sprintf("%.0f%%", model$summary$specificity * 100),
    AUC = sprintf("%.2f", model$summary$AUC),
    `Odds Ratio` = paste(
      publish(model$model, print = FALSE)$regressionTable$OddsRatio,
      gsub(";", "-", publish(model$model, print = FALSE)$regressionTable$CI.95)
    ),
    tp = summary(model$cutpoint)$confusion_matrix[[1]]$tp,
    fn = summary(model$cutpoint)$confusion_matrix[[1]]$fn,
    fp = summary(model$cutpoint)$confusion_matrix[[1]]$fp,
    tn = summary(model$cutpoint)$confusion_matrix[[1]]$tn,
    check.names = FALSE
  ) %>%
    mutate(
      sens = tp / (tp + fn),
      spec = tn / (fp + tn),
      sen = sprintf("%.0f%%", tp / (tp + fn) * 100),
      spe = sprintf("%.0f%%", tn / (fp + tn) * 100),
      ppv = sprintf("%.0f%%", tp / (tp + fp) * 100),
      npv = sprintf("%.0f%%", tn / (fn + tn) * 100),
      plr = sprintf("%.1f", sens / (1 - spec)),
      nlr = sprintf("%.1f", (1 - sens) / spec)
    ) %>%
    select(-sens, -spec)
}

model_ssAOCI_summary <- function(predictor_models) {
  table <- lapply(predictor_models, function(predictor_model) {
    optim_row <- model_ssAOCI(predictor_model$optim_model)
    secondary_rows <- lapply(predictor_model$secondary_models, model_ssAOCI) %>%
      bind_rows()
    return(rbind(optim_row, secondary_rows))
  }) %>%
    bind_rows()
}

# Table 3: Multivariable models with patient characteristics and creatinine change for the prediction of stage 2 or 3 AKI
model_nri <- function(data, outcome, std_vars, new_vars) {
  calc_nri <- nribin(
    event = data[[outcome]],
    # TODO: Change to p.std, and p.new where p.std needs to be calculated......
    z.std = as.matrix(select(data, all_of(std_vars))),
    z.new = as.matrix(select(data, all_of(new_vars))),
    cut = 0.078, # multi_model$optim_model$cutpoint$optimal_cutpoint,
    # TODO: implement this as a function argument
    msg = FALSE,
    updown = "diff"
  )
  calc_nri$nri %>%
    rownames_to_column("Var") %>%
    slice_head(n = 3) %>%
    mutate(CI = sprintf("%.2f [%.2f-%.2f]", Estimate, Lower, Upper)) %>%
    select(Var, CI) %>%
    pivot_wider(names_from = Var, values_from = CI)
}


model_ssACIBnri <- function(model, std_vars) {
  outcome <- gsub(" ~.*", "", model$summary$glm_model)
  predictors <- as.vector(str_split(gsub(".*~ ", "", model$summary$glm_model), " \\+ ", simplify = TRUE))

  boot_cut <- cutpointr(
    model$data,
    predict, {{ outcome }},
    use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
    method = maximize_metric, metric = youden,
    boot_runs = 1000
  )
  AUC_ci <- boot_ci(boot_cut, AUC, in_bag = TRUE, alpha = 0.05)

  calc_nri <- suppressMessages(model_nri(
    model$data, outcome, std_vars, predictors
  ))

  data.frame(
    GLM = model$summary$glm_model,
    Sensitivity = sprintf("%.0f%%", model$summary$sensitivity * 100),
    Specificity = sprintf("%.0f%%", model$summary$specificity * 100),
    AUC = sprintf("%.2f [%.2f-%.2f]", model$summary$AUC, AUC_ci$values[1], AUC_ci$values[2]),
    `Brier Score` = sprintf("%.3f", BrierScore(model$model)),
    check.names = FALSE
  ) %>%
    cbind(., calc_nri)
}

model_ssACIBnri_summary <- function(multi_model, baseline_model) {
  baseline_vars <- baseline_model$summary$glm_model
  baseline_vars <- as.vector(str_split(gsub(".*~ ", "", baseline_vars), " \\+ ", simplify = TRUE))

  # TODO: Instead of using baseline_vars for z.std, I should be
  # creating a new glm and creating the prediction column. Then use p.std in nribin

  models <- c(
    multi_model$baseline_models,
    list(
      optim_model_full = multi_model$optim_model_full,
      optim_model = multi_model$optim_model
    ) # ,
    # multi_model$secondary_models
  )

  # cl <- NULL # makeCluster(detectCores() - 1)
  table <- pblapply(models, model_ssACIBnri, baseline_vars) %>%
    bind_rows()
  # stopCluster(cl)

  return(table)
}

# nribin(
#   event = multi_model$optim_model$data$AKI_2or3,
#   z.std = as.matrix(select(
#     multi_model$optim_model$data,
#     PCs_cardio, Vasopressor, Chronic_liver_disease
#   )),
#   z.new = as.matrix(select(
#     multi_model$optim_model$data,
#     PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient
#   )),
#   cut = 0.1, # multi_model$baseline_models$baseline_sig$cutpoint$youden,
#   msg = TRUE,
#   updown = "diff"
# )
#
# nribin(
#   event = multi_model$optim_model$data$AKI_2or3,
#   z.std = as.matrix(select(
#     multi_model$optim_model$data,
#     APACHE_II, PCs_cardio, Vasopressor
#   )),
#   z.new = as.matrix(select(
#     multi_model$optim_model$data,
#     PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient
#   )),
#   cut = 0.1, # multi_model$baseline_models$baseline_sig$cutpoint$youden,
#   msg = TRUE,
#   updown = "diff"
# )
