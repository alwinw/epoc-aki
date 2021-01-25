# fr <- function(x) {
#   value <- 100 * sum((1 - x)^2)
#   penalty <- sum((round(x) - x)^2)
#   value + 1e5 * penalty
# }
# fnmap_f <- function(x) c(x[1:(length(x) - 1)], round(tail(x, 1)))
# test_optim = DEoptim(
#   fr, 
#   lower = c(-3,-3,-3), 
#   upper = c(3,3,3), 
#   control = DEoptim.control(trace = FALSE, parallelType = 1),
#   fnMap = fnmap_f 
# )
# test_optim$optim$bestmem
# plot(test_optim)
