libs <- c("here", "purrr")
suppressMessages(
  suppressWarnings(sapply(libs, require, character.only = TRUE))
)
rm(libs)

sample_data <- read.csv(here("data", "sample_ga_data_binomial_2019-04-26.csv"))
source(here("R", "sim_variables.R"))
source(here("R", "ga_gbm_function.R"))
source(here("R", "vimshift_ph290_project.R"))

concordance <- function(l1, l2) {
  con <- map_dbl(1:length(l1), function(i) {
    sum(l1[1:i] %in% l2[1:i])}
  )
  return(sum(con))
}

library("doParallel")
nCores <- 10
reps <- 10
registerDoParallel(nCores)

k <- 7
n <- 3^k

print("Running for various seed for n")
results <- foreach(i = 1:reps) %dopar% {
  tryCatch(
  {sim1 <- simulate_outcome(sample_data = sample_data, n = n,
                           complexity = 1, seed = sample(1:10000, 1))
  trueOrder <- ranking_complex(ranking_info = sim1$ranking_info)
  gbOrder <- gbm(data = data.frame(sim1$covariates, y = sim1$y))
  suppressMessages(vimshiftOrder <- run_combined_var_imp(train = data.frame(sim1$covariates,
                                                           y = as.integer(sim1$y)),
                                        Wnames, Wnames_cat))
  c(concordance(names(sort(trueOrder, decreasing = TRUE)),
                names(sort(gbOrder, decreasing = TRUE))),
    concordance(names(sort(trueOrder, decreasing = TRUE)), vimshiftOrder))}
  )
}

saveRDS(results, file = here("data", paste0("n_", n, ".rds")))
complexity <- 5
print("Running for various seed for complexity")
complexity <- 1
results <- foreach(i = 1:reps) %dopar% {
  tryCatch(
  {sim1 <- simulate_outcome(sample_data = sample_data, n = 1000,
                           complexity = complexity, seed = sample(1:10000, 1))
  trueOrder <- ranking_complex(ranking_info = sim1$ranking_info)
  gbOrder <- gbm(data = data.frame(sim1$covariates, y = sim1$y))
  suppressMessages(vimshiftOrder <- run_combined_var_imp(train = data.frame(sim1$covariates,
                                                           y = as.integer(sim1$y)),
                                        Wnames, Wnames_cat))
  c(concordance(names(sort(trueOrder, decreasing = TRUE)),
                names(sort(gbOrder, decreasing = TRUE))),
    concordance(names(sort(trueOrder, decreasing = TRUE)), vimshiftOrder))}
  )
}

saveRDS(results, file = here("data", paste0("complexity_", complexity, ".rds")))
