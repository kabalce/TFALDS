MLE_of_beta <- function(X) {
  n <- length(X)
  return((-1) * n / sum(sapply(X, log)) - 1)
}

moment_estimator_of_beta <- function(X) {
  u <- mean(X)
  return((1 - 2 * u) / (u - 1))
}

provide_estimators_1 <- function(a, n) {
  X <- rbeta(n, a + 1, 1)
  mle <- MLE_of_beta(X)
  mom <- moment_estimator_of_beta(X)
  return(array(c(mle, mom, mle - a, mom - a, (mle - a) ^ 2, (mom - a) ^ 2), c(2, 3)))
}

simulation_1 <- function(a, n, m) {
  # dim1  estimator
  # dim2  statistic
  # dim3  iteration
  replicate(m, provide_estimators_1(a, n))
}

conf_int_bias <- function(a_vec, a, m, alph=0.05) {
  bs <- a_vec - a
  b <- mean(bs)
  b_sd <- sd(bs)
  return (list(conf_int_lower = b - qnorm(1 - alph / 2) * b_sd / sqrt(m),
               conf_int_upper = b + qnorm(1 - alph / 2) * b_sd / sqrt(m), 
               est = b))
}

conf_int_mse <- function(a_vec, a, m, alph=0.05) {
  mses <- (a_vec - a) ^ 2
  mse <- mean(mses)
  mse_sd <- sd(mses)
  return (list(conf_int_lower = mse - qnorm(1 - alph / 2) * mse_sd / sqrt(m), 
               conf_int_upper = mse + qnorm(1 - alph / 2) * mse_sd / sqrt(m), 
               est = mse))
}

conf_int_var <- function(a_vec, a, m, alph=0.05) {
  v <- var(a_vec)
  return (list(conf_int_lower = (m - 1) * v / qchisq(1 - alph / 2, m - 1), 
               conf_int_upper = (m - 1) * v / qchisq(alph / 2, m - 1), 
               est = v))
}


## test
# array20 <- simulation_1(5, 20, 1000)

## what is the asymptotic distribution of MLE???
## przedziały ufności dla biad, variance... (z CTG ??)

# Bias: mean(a_mle - a)
# variance: 1 / (n I(a))
# conf int from nromal distr

# koło siebie w raporcie dwa histogramy estymatorów tego samego parametru, zwróc uwagę na skale

# raport: wyniki, komentarze, kod; zadania teoretyczne też do raportu