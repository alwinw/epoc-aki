# Manual logistic predictor using 1/(1+e^-y)
manual_predictor <- function(PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient) {
  y <- coef(multi_model$optim_model$model)["(Intercept)"] +
    coef(multi_model$optim_model$model)["PCs_cardio"] * PCs_cardio +
    coef(multi_model$optim_model$model)["Vasopressor"] * Vasopressor +
    coef(multi_model$optim_model$model)["Chronic_liver_disease"] * Chronic_liver_disease +
    coef(multi_model$optim_model$model)["cr_gradient"] * cr_gradient
  as.numeric(1 / (1 + exp(-y)))
}

stopifnot(all.equal(
  manual_predictor(
    multi_model$optim_model$data$PCs_cardio,
    multi_model$optim_model$data$Vasopressor,
    multi_model$optim_model$data$Chronic_liver_disease,
    multi_model$optim_model$data$cr_gradient
  ),
  as.numeric(multi_model$optim_model$data$predict)
))


# Search for optimum score coefficients based on BrierScore

BrierScore(multi_model$optim_model$data$AKI_2or3, multi_model$optim_model$data$predict)

brier_wrapper <- function(b, data) {
  y <- 0 + b[1] +
    b[2] * data$PCs_cardio +
    b[3] * data$Vasopressor +
    b[4] * data$Chronic_liver_disease +
    b[5] * data$cr_gradient
  p <- as.numeric(1 / (1 + exp(-y)))
  BrierScore(data$AKI_2or3, p)
}

# brier_wrapper(c(-4.9068747, 1.7988061, 1.1770319, 3.6879612, 0.9126502), multi_model$optim_model$data)

score_coef <- DEoptim(
  brier_wrapper,
  rep(-10, 5),
  rep(10, 5),
  DEoptim.control(NP = 320, itermax = 500, trace = 100),
  fnMap = function(x) c(x[1], round(x[2:5], 0)),
  data = multi_model$optim_model$data
)
score_coef$optim$bestval
score_coef$optim$bestmem
# Summary: Vasopressor is 3x more important than others


# Visualise the suggested score
temp <- multi_model$optim_model$data %>%
  select(AKI_2or3, predict, PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient) %>%
  mutate(score = PCs_cardio + Vasopressor + 3 * Chronic_liver_disease + cr_gradient) %>%
  group_by(score)

temp %>% summarise(
  # across(everything(), list(mean = mean, median = median))
  mean = mean(predict) * 100,
  median = median(predict) * 100,
  .groups = "drop"
)

ggplot(temp, aes(x = score, y = predict, group = score)) +
  geom_point() +
  geom_boxplot()

score_est <- score_predictor(
  multi_model$optim_model$data$PCs_cardio,
  multi_model$optim_model$data$Vasopressor,
  multi_model$optim_model$data$Chronic_liver_disease,
  multi_model$optim_model$data$cr_gradient
)
