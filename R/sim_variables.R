library(here)
library(purrr)

simulate_covariate <- function(data, n, type) {
  if (type == "bern") {
      return(sample(x = data, size = n, replace = TRUE))
  }
  if (type == "multi") {
    return(sample(x = data, size = n, replace = TRUE) +
             runif(n, -.0005, 0.0005))
  }
  if (type == "pois") {
    return(rpois(n = n, lambda = mean(data)) +
           runif(n, -.0005, 0.0005))
  }
  if (type == "norm") {
    return(rnorm(n = n, mean = mean(data), sd = sd(data)))
  }
  if (type == "lnorm") {
    return(exp(rnorm(n = n, mean = mean(log(data)), sd = sd(log(data)))))
  }
}

simulate_covariates <- function(sample_data, n) {
  # Define the distribution functions for each variable
  types <- c("multi", "lnorm", "lnorm", "norm", "norm", "lnorm", "norm", "norm",
             "norm", "pois", "bern", "pois", "pois", "pois", "bern",
             "bern", "bern", "bern", "bern", "bern", "bern", "bern", "norm",
             "pois", "pois", "multi", "multi")
  # For each column, simulate
  return(map2_df(sample_data[, -c(1, 29)], types, .f = simulate_covariate, n = n))
}

preds <- function(k, complexity) {
  interactions <- sample(k * (k - 1) / 2,
                         size = complexity * 2,
                         replace = FALSE)
  interactions <- combn(1:k, 2)[, interactions]
  powers <- sample(1:k, size = complexity, replace = FALSE)
  degrees <- sample(c(1/2, 2, 3), size = complexity, replace = TRUE)
  betas <- rnorm(k + 3 * complexity, sd = 3)
  prob <- function(cov) {
    odd <- sum(betas[1:length(cov)] * cov)
    if (complexity > 0) {
      for (i in 1:complexity) {
        odd <- odd + abs(cov[powers[i]])^degrees[i] * abs(betas[k + i]) *
          sign(betas[powers[i]])
      }
      for (i in 1:(2 * complexity)) {
        odd <- odd + cov[interactions[1, i]] * cov[interactions[2, i]] *
                     betas[k + complexity + i]
      }
    }
    return(1 / (1 + exp(-odd)))
  }
  return(list(interactions = interactions,
              powers = powers,
              betas = betas,
              prob = prob,
              degrees = degrees))
}

simulate_outcome <- function(sample_data, n, complexity = 0, seed = 47291) {
  set.seed(seed)
  covariates_init <- simulate_covariates(sample_data = sample_data, n = n)
  covariates <- scale(covariates_init, center = TRUE, scale = TRUE)
  prediction <- preds(k = ncol(covariates), complexity = complexity)
  pred_prob <- prediction$prob
  y <- apply(covariates, 1, function(cov){
    return(rbernoulli(1, p = pred_prob(cov)))
  })
  if (mean(y) < .05 | mean(y) > .95) {
    return(simulate_outcome(sample_data = sample_data, n = n))
  } else {
    prediction[["covariates_init"]] <- covariates_init
    return(list(y = y,
                covariates = covariates_init,
                ranking_info = prediction
                )
           )
  }
}

ranking_simple <- function(ranking_info) {
  interactions <- ranking_info$interactions
  powers <- ranking_info$powers
  betas <- ranking_info$betas
  degrees <- ranking_info$degrees
  complexity <- length(powers)
  k <- length(betas) - 3 * complexity
  strength <- betas[1:k]
  if (complexity > 0) {
    for (i in 1:complexity) {
      strength[power[i]] <- strength[power[i]] + betas[k + i]
    }
    for (i in 1:(2 * complexity)) {
      strength[interactions[1, i]] <- strength[interactions[1, i]] + betas[k + i + complexity]
      strength[interactions[2, i]] <- strength[interactions[2, i]] + betas[k + i + complexity]
    }
  }
  return(strength)
}

ranking_complex <- function(ranking_info) {
  k <- ncol(ranking_info$covariates_init)
  interactions <- ranking_info$interactions
  powers <- ranking_info$powers
  betas <- ranking_info$betas
  degrees <- ranking_info$degrees
  complexity <- length(powers)
  strength <- rep(0, k)
  covariates <- ranking_info$covariates_init
  for (i in 1:k) {
    linear_effect <- betas[i] * covariates[, i]
    if (complexity > 0) {
      if (i %in% powers) {
        j <- which(powers == i)
        power_effect <- betas[k + j] * (covariates[, i])^(degrees[j])
      } else {
        power_effect <- 0
      }
      interaction_effect <- 0
      if (i %in% interactions) {
        for (j in 1:ncol(interactions)) {
          if (i %in% interactions[,j]) {
            interaction_effect <- interaction_effect +
              betas[k + j + complexity] * covariates[, interactions[1, j]] *
              covariates[, interactions[2, j]]
          }
        }
      }
    } else {
      power_effect <- 0
      interaction_effect <- 0
    }
    effect <- linear_effect[, 1] + power_effect + interaction_effect
    strength[i] <- quantile(unlist(effect), 0.8) -
                   quantile(unlist(effect), 0.2)
  }
  names(strength) <- colnames(covariates)
  return(strength)
}
