library(readr)
library(here)

file <- read_lines(here("R", "simulation_file.R"))
for (i in 1:7) {
  f <- file
  f[24] <- paste0("k <- ", 5 + i)
  f[44] <- paste0("complexity <- ", 7 - i)
  write_lines(f, path = here("R", paste0("simulation_", i, ".R")))
}

file <- read_lines(here("scripts", "run.sh"))
for (i in 1:7) {
  f <- file
  f[9] <- paste0("file='R/simulation_", i, "'")
  write_lines(f, path = here("scripts", paste0("simulation_", i, ".sh")))
}
