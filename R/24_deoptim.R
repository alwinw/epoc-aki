# fr <- function(x) {
#   value <- 100 * sum((1 - x)^2)
#   penalty <- sum((round(x) - x)^2)
#   value + 1e5 * penalty
# }
# fnmap_f <- function(x) c(x[1:(length(x) - 1)], round(tail(x, 1)))
# test_optim <- DEoptim(
#   fr,
#   lower = c(-3, -3, -3),
#   upper = c(3, 3, 3),
#   control = DEoptim.control(trace = FALSE, parallelType = 1),
#   fnMap = fnmap_f
# )
# test_optim$optim$bestmem
# plot(test_optim)

heuristic_calc <- function(AUC, per_admin_in) -AUC

# Move calc to here so reusable later

heuristic_search <- function(x,
                             outcome_var = "AKI_ICU",
                             baseline_predictors = c("Age"),
                             cr_predictors = NULL,
                             del_t_ch_hr_range = NULL,
                             del_t_aki_hr_range = NULL,
                             add_gradient_predictor = NULL,
                             stepwise = FALSE,
                             k = "mBIC",
                             plot_cutpoint = FALSE,
                             # heuristic_only = FALSE,
                             all_data = FALSE,
                             analysis_data = analysis_df) {
  ch_min <- x[1] - x[2] / 2
  ch_max <- x[1] + x[2] / 2
  aki_min <- x[3]
  aki_max <- x[3] + x[4]

  heuristic <- tryCatch(
    {
      output <- aki_dev_wrapper(
        outcome_var = outcome_var,
        baseline_predictors = baseline_predictors,
        cr_predictors = cr_predictors,
        del_t_ch_hr_range = c(ch_min, ch_max),
        del_t_aki_hr_range = c(aki_min, aki_max),
        add_gradient_predictor = add_gradient_predictor,
        stepwise = stepwise,
        k = k,
        heuristic_only = TRUE,
        analysis_data = analysis_df
      )
      heuristic_calc(output$AUC, output$per_admin_in)
    },
    error = function(e) {
      warning(e)
      Inf
    }
  )
  return(heuristic)
}

heuristic_optim <- DEoptim(heuristic_search,
  lower = c(4, 0.5, 24, 1),
  upper = c(6, 3, 48, 48),
  control = DEoptim.control(itermax = 10)
)

x <- heuristic_optim$optim$bestmem
ch_min <- x[1] - x[2] / 2
ch_max <- x[1] + x[2] / 2
aki_min <- x[3]
aki_max <- x[3] + x[4]

aki_dev_wrapper(
  outcome_var = "AKI_ICU",
  baseline_predictors = c("Age"),
  cr_predictors = NULL,
  del_t_ch_hr_range = c(ch_min, ch_max),
  del_t_aki_hr_range = c(aki_min, aki_max),
  add_gradient_predictor = NULL,
  stepwise = FALSE,
  k = "mBIC",
  plot_cutpoint = FALSE,
  heuristic_only = FALSE,
  all_data = FALSE,
  analysis_data = analysis_df
)
