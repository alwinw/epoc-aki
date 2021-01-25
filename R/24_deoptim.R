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

# ---- wrapper functions ----
heuristic_wrapper <- function(
                              x,
                              penalty_fn = function(x) x$penalty,
                              return_fn = function(x) x,
                              ...) {
  tryCatch(
    {
      summary <- aki_dev_wrapper(
        del_t_ch_hr_range = c(x[1] - x[2] / 2, x[1] + x[2] / 2),
        del_t_aki_hr_range = c(x[3], x[3] + x[4]),
        ...
      )
      summary$penalty <- penalty_fn(summary)
      return(return_fn(summary))
    },
    error = function(e) {
      warning(e)
      summary <- data.frame(penalty = Inf)
      return(return_fn(summary))
    }
  )
}
# heuristic_wrapper(
#   c(6, 1, 9, 40),
#   penalty_fn = function(summary) 1 - summary$AUC,
#   return_fn = function(summary) summary$penalty
# )

deoptim_wrapper <- function(lower, upper, itermax, ..., return_fn, fnMap) {
  # cl <- makeCluster(detectCores() - 1)
  # invisible(clusterEvalQ(cl, library("dplyr")))
  # invisible(clusterEvalQ(cl, library("cutpointr")))
  # clusterExport(cl, c("analysis_df", "aki_dev_wrapper"))

  result <- DEoptim(
    heuristic_wrapper,
    lower = lower,
    upper = upper,
    control = DEoptim.control(
      itermax = itermax # ,
      # parallelType = 1,
      # packages = c("dplyr", "cutpointr"),
      # parVar = c("analysis_df", "aki_dev_wrapper")
    ),
    ...,
    return_fn = return_fn,
    fnMap = fnMap
  )
  return(heuristic_wrapper(result$optim$bestmem, ...))
}


# ---- cr_ch_only ----
set.seed(8)

cr_ch_optim <- deoptim_wrapper(
  lower = c(4, 0.5, 24, 1),
  upper = c(6, 3, 48, 48),
  itermax = 2,
  outcome_var = "AKI_2or3",
  baseline_predictors = "",
  cr_predictors = "",
  add_gradient_predictor = 1,
  penalty_fn = function(summary) 1 - summary$AUC,
  return_fn = function(summary) summary$penalty,
  fnMap = function(x) round(x, 1)
)
