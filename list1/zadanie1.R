# beta
# 

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


## test
# array20 <- simulation_1(5, 20, 1000)

## what is the asymptotic distribution of MLE???
## przedziały ufności dla biad, variance... (z CTG ??)

# Bias: mean(a_mle - a)
# variance: 1 / (n I(a))
# conf int from nromal distr

# koło siebie w raporcie dwa histogramy estymatorów tego samego parametru, zwróc uwagę na skale

# raport: wyniki, komentarze, kod; zadania teoretyczne też do raportu