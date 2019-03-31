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

# We have to remove two variables because there is not enought support in the
# data to consider them continuous before binning them. It migth also happen for
# tmleshift, we will see
vim <- varimpact(Y = as.integer(df$y),
                 data = df %>% select(-y, -dhc, -fun_ht_anc1) %>% as.data.frame())


