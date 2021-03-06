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
nCores <- 2
reps <- 3
registerDoParallel(nCores)

n = 500
print("Running for various seed for n")
results <- foreach(i = 1:reps) %dopar% {
  suppressMessages(suppressWarnings(tryCatch(
    {sim1 <- simulate_outcome(sample_data = sample_data, n = n,
                              complexity = 1, seed = sample(1:10000, 1))
    trueOrder <- ranking_complex(ranking_info = sim1$ranking_info)
    gbOrder <- gbm(data = data.frame(sim1$covariates, y = sim1$y))
    suppressMessages(vimshiftOrder <- run_combined_var_imp(train = data.frame(sim1$covariates,
                                                                              y = as.integer(sim1$y)),
                                                           Wnames, Wnames_cat))
    a <- list("truth" = trueOrder,
              "gb" = gbOrder,
              "vim" = vimshiftOrder)
    saveRDS(a, file = here("data", paste0("n_", n, "_", i, ".rds")))
    a
    })))
  cat("Iteration", i, "done\n")
}
