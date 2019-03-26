# Load packages ----
libs <- c("varimpact", "stringr", "devtools", "here", "readr", "tidyr",
          "dplyr")
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
df <- read_csv(here("data", "sample_ga_data_binomial.csv"),
               col_types = paste0(rep("n", 23), collapse = ""))[, -1]
vim <- varimpact(Y = as.integer(df$y),
                 data = df %>% select(-y) %>% as.data.frame())


