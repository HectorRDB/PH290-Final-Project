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
               col_types = paste0(rep("n", 22), collapse = ""))[, -1] %>%
  mutate(m_stature = factor(m_stature),
         fuel = factor(fuel),
         enough_food = factor(enough_food),
         ever_no_food = factor(ever_no_food),
         run_out_food = factor(run_out_food),
         not_enough_food = factor(not_enough_food),
         hh_smoker = factor(hh_smoker),
         alc = factor(alc),
         sex = factor(sex))

# We have to remove two variables because there is not enought support in the
# data to consider them continuous before binning them. It migth also happen for
# tmleshift, we will see
vim <- varimpact(Y = as.integer(df$y),
                 data = df %>% select(-y, -dhc, -fun_ht_anc1) %>% as.data.frame())
rm(df)

