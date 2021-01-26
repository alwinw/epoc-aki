# ---- wrapper functions ----
heuristic_penalty <- function(summary) {
  0 +
    (1 - summary$AUC) +
    (1 - summary$per_admin_in) * 2 +
    (1 - summary$per_admin_pos) * 3
  # other
}

heuristic_wrapper <- function(
                              x,
                              penalty_fn = function(x) x$penalty,
                              return_fn = function(x) x,
                              ...) {
  summary <- aki_dev_wrapper(
    del_t_ch_hr_range = c(x[1] - x[2] / 2, x[1] + x[2] / 2),
    del_t_aki_hr_range = c(x[3], x[3] + x[4]),
    ...
  )
  summary$penalty <- penalty_fn(summary)
  return(return_fn(summary))
}
# heuristic_wrapper(
#   c(6, 1, 6, 12),
#   outcome_var = "AKI_ICU",
#   penalty_fn = function(summary) 1 - summary$AUC,
#   return_fn = function(summary) summary$penalty
# )

deoptim_wrapper <- function(
                            lower, upper,
                            itermax, ...,
                            return_fn = function(summary) summary$penalty,
                            fnMap = function(x) round(x, 1)) {
  result <- DEoptim(
    heuristic_wrapper,
    lower = lower,
    upper = upper,
    control = DEoptim.control(
      itermax = itermax,
      NP = 100,
      reltol = 1e-5,
      parallelType = 1,
      packages = c("dplyr", "cutpointr"),
      parVar = c("analysis_df", "aki_dev_wrapper")
    ),
    ...,
    return_fn = return_fn,
    fnMap = fnMap
  )
  return(list(
    bestmem = heuristic_wrapper(result$optim$bestmem, ...),
    result = result
  ))
}

# ---- cr_ch_only ----
set.seed(8)
cr_ch_optim <- deoptim_wrapper(
  lower = c(4, 0.5, 3, 1),
  upper = c(6, 6, 12, 48),
  itermax = 50,
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = NULL,
  add_gradient_predictor = 1,
  penalty_fn = heuristic_penalty
)
cr_ch_optim$bestmem

heuristic_wrapper(cr_ch_optim$result$optim$bestmem, outcome_var = "AKI_2or3",
                  baseline_predictors = "",
                  cr_predictors = "",
                  add_gradient_predictor = 1)
