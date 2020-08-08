

# ---- example_cont_1 ----
example_cont_1 <- generate_example(
  crch_centre = 4,
  t_interval_width = 3,
  min_hr_until_aki = 8,
  max_hr_until_aki = 48,
  add_gradient_predictor = NULL
  )
kable(publish(example_cont_1$model, print = FALSE)$regressionTable)
plot(example_cont_1$cutpoint)

# ---- example_cont_2 ----
example_cont_2 <- generate_example(
  crch_centre = 6.5,
  t_interval_width = 1,
  min_hr_until_aki = 8,
  max_hr_until_aki = 16)
kable(publish(example_cont_2$model, print=FALSE)$regressionTable)
plot(example_cont_2$cutpoint)
