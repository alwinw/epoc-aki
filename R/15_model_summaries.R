# Summarise into various tables
# One for cr_gradient, one for percentage change
# Multivariable models for different outcomes
# Add a plot showing cr vs time and predictive value


# Table 2 – Predictive value parameters for creatinine change as an independent predictor of AKI

model_ssAOCI <- function(model) {
  data.frame(
    GLM.Model = model$summary$glm_model,
    Cr.ch.epis.duration = paste0(model$summary$ch_hr_lower, "-", model$summary$ch_hr_upper),
    Outcome = gsub(" ~.*", "", model$summary$glm_model),
    Timeframe.to.AKI = paste0(model$summary$aki_hr_lower, "-", model$summary$aki_hr_upper),
    Sensitivity = model$summary$sensitivity,
    Specificity = model$summary$specificity,
    AUC = model$summary$AUC,
    Odds.Ratio = publish(model$model, print = FALSE)$regressionTable$OddsRatio,
    CI.95 = publish(model$model, print = FALSE)$regressionTable$CI.95
  )
}
