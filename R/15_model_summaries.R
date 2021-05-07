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
    check.names = FALSE
  )
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
