# Load packages ----
libs <- c("varimpact", "stringr", "devtools", "here", "readr")
pckgs <- suppressMessages(
  suppressWarnings(sapply(libs, require, character.only = TRUE))
)
rm(libs)

if (!pckgs[1]) {
  install_github("ck37/varimpact")
  library("varimpact")
}
rm(pckgs)

# Load data ----
df <- read_csv(here("data", "sample_ga_data_binomial.csv"))
