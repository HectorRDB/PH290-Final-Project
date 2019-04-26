library(here)
library(purrr)

simulate_covariate <- function(data, n, type) {
  if (type == "bern") {
      return(sample(x = data, size = n, replace = TRUE))
  }
  if (type == "multi") {
    return(sample(x = data, size = n, replace = TRUE))
  }
  if (type == "pois") {
    return(rpois(n = n, lambda = mean(data)))
  }
  if (type == "norm") {
    return(rnorm(n = n, mean = mean(data), sd = sd(data)))
  }
}

simulate_covariates <- function(sample_data, n) {
  # Define the distribution functions for each variable
  types <- c("multi", "norm", "pois", "bern", "pois", "pois", "pois", "bern",
             "bern", "bern", "bern", "bern", "bern", "bern", "bern", "norm",
             "pois", "pois", "multi", "multi")  
  # For each column, simulate
  return(map2_df(sample_data[, -c(1, 22)], types, .f = simulate_covariate, n = n))
}

preds <- function(k, complexity) {
  interactions <- sample(k * (k - 1) / 2,
                         size = complexity * 2,
                         replace = FALSE)
  interactions <- combn(1:k, 2)[, interactions]
  powers <- sample(1:k, size = complexity, replace = TRUE)
  degrees <- sample(c(1/3, 1/2, 2, 3, 2/3), size = complexity, replace = TRUE)
  betas <- rnorm(k + 3 * complexity)
  prob <- function(cov) {
    odd <- sum(betas[1:length(cov)] * cov)
    if (complexity > 0) {
      for (i in 1:complexity) {
        odd <- odd + abs(cov[powers[i]])^degrees[i] * betas[k + i]
      }
      for (i in 1:(2 * complexity)) {
        odd <- odd + cov[interactions[1, i]] * cov[interactions[2, i]] *
                     betas[k + complexity + i]
      }
    }
    return(1 / (1 + exp(-odd)))
  }
}

simulate_outcome <- function(sample_data, n, complexity = 0, seed = 47291) {
  set.seed(seed)
  covariates_init <- simulate_covariates(sample_data = sample_data, n = n)
  covariates <- scale(covariates_init, center = TRUE, scale = TRUE)
  pred_prob <- preds(k = ncol(covariates), complexity = complexity)
  y <- apply(covariates, 1, function(cov){
    return(rbernoulli(1, p = pred_prob(cov)))
  })
  if (mean(y) < .05 | mean(y) > .95) {
    return(simulate_outcome(sample_data = sample_data, n = n))
  } else {
    return(list(y = y, covariates = covariates_init))
  }
}
