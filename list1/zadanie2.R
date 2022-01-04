provide_estimators_2 <- function(n, lambda) {
  X <- rexp(n, lambda)
  T_stat <- sum(X)
  p_value <- 1 - pgamma(T_stat, n, 5)
  return(p_value)
}

conf_int_2 <- function(p, alph) {
  p_est <- mean(p < alph)
  return(list(est = p_est, 
              conf_int_lower = p_est - qnorm(1 - alph / 2) * sqrt(p_est * (1 - p_est) / length(p)),
              conf_int_upper = p_est + qnorm(1 - alph / 2) * sqrt(p_est * (1 - p_est) / length(p))))
}

simulation_2 <- function(lambda, n, m) {
  # dim1  estimator
  # dim2  value
  return(replicate(m, provide_estimators_2(n, lambda)))
}

res <- simulation_2(5, 20, 1000)
conf_int_2(res, 0.05)

res <- simulation_2(3, 20, 1000)
conf_int_2(res, 0.05)
