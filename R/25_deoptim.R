# ---- wrapper functions ----
tanh_penalty <- function(x, c, d, s) 1 / 2 + 1 / 2 * tanh(s / d * atanh(0.8) * (x - c))

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
    del_t_ch_hr_range = c(x[1] - x[2] / 2, x[1] + x[2] / 2),
    del_t_aki_hr_range = c(x[3], x[3] + x[4]),
    ...
  )
  summary$penalty <- penalty_fn(summary)
  return(return_fn(summary))
}

deoptim_wrapper <- function(
                            lower, upper,
                            itermax, ...,
                            return_fn = function(summary) summary$penalty,
                            fnMap = function(x) round(x, 1),
                            parallel = FALSE) {
  if (parallel) {
    control <- DEoptim.control(
      itermax = itermax,
      NP = 320,
      reltol = 1e-5,
      parallelType = 1,
      packages = c("dplyr", "cutpointr"),
      parVar = c("analysis_df", "aki_dev_wrapper", "heuristic_penalty", "tanh_penalty")
    )
  } else {
    control <- DEoptim.control(
      itermax = itermax,
      NP = 320,
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

# ---- deoptim functions ----
deoptim_search <- function(...) {
  set.seed(8)
  
}
