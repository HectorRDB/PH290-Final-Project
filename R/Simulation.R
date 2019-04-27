#Questions:
# 1. data size - the same?
# 2. same mean, standard deviation etc.?
# 3. binomial or continuous?

library(stats)
library(here)
library(texreg)
library(simstudy)
#sample_data <- read.csv(here("data", "sample_ga_data_binomial.csv"))
sample_data <- read.csv(here("data", "sample_ga_data_binomial.csv"))
# head(sample_data)
# summary(sample_data)
n <- 1000


library(fitdistrplus)
library(logspline)


disc_dist <- function(x){
  #descdist(x, boot = 1000, discrete = TRUE)
  # looks like neg binomial or poisson
  fit_pois <- try(fitdist(x, "pois"))
  if (is(fit_pois) == "try-error") {
    fit_pois <- NA
  } else {
   fit_pois <- fit_pois$aic
  }
  fit_nbinom <- try(fitdist(x, "nbinom"))
  if (is(fit_nbinom) == "try-error") {
    fit_nbinom <- NA
  } else {
    fit_nbinom <- fit_nbinom$aic
  }
  fit_norm <- try(fitdist(x, "norm"))
  if (is(fit_norm) == "try-error") {
    fit_norm <- NA
  } else {
    fit_norm <- fit_norm$aic
  }

  aic <- c("Poisson" = fit_pois, "Negative Binomial" = fit_nbinom,
           "Normal" = fit_norm)
  return(aic)
}

df <- data.frame("Poisson" = rep(NA, 21), "Negative Binomial" = rep(NA, 21),
                 "Normal" = rep(NA, 21), "LogNormal" = rep(NA, 21),
                 "mean" = rep(NA, 21), "sd" = rep(NA, 21), "pr" = rep(NA, 21))
rownames <- c()
for (i in c(3:18)) {
  fit <- disc_dist(sample_data[, i])
  df[i - 2, ] <- c(fit, NA, mean(sample_data[, i]),
                   sd(sample_data[, i]), NA)
  rownames <- c(rownames, colnames(sample_data[i]))
}

for (i in c(19:22)) {
  fit <- disc_dist(sample_data[, i])
  df[i - 2, ] <- c(fit, NA, mean(sample_data[, i]), sd(sample_data[, i]), NA)
  # df <- rbind(df, fit)
  rownames <- c(rownames, colnames(sample_data[i]))
}

# to get the probability for the binary variables
for (i in c(8:15)) {
  y <- sum(sample_data[, i + 2])
  df[i, 7] <- y / 384
}

df[19, 7] <- sum(sample_data[, 23]) / 384
# df <- cbind(df, rep(NA, 19))
# df <- cbind(df, rep(0, 19))
# df <- cbind(df, rep(0, 19))
# colnames(df) <- c("Poisson", "Negative Binomial", "Normal", "LogNormal", "mean", "sd")
df


# for the two continuous variables head_cir and length_b
x <- sample_data[, 19]
descdist(x, boot = 1000, discrete = FALSE)
# looks like lognormal

fit_19 <- fitdist(x, "lnorm")
df[20, ] <- c(NA, NA, NA, fit_19$aic, log(mean(sample_data[, 19])), log(sd(sample_data[, 19])), NA)
rownames <- c(rownames, colnames(sample_data[19]))

# for sample length_b
x <- sample_data[, 20]
descdist(x, boot = 1000, discrete = FALSE)
# looks like lognormal

fit_20 <- fitdist(x, "lnorm")
df[21, ] <- c(NA, NA, NA, fit_20$aic, log(mean(sample_data[, 20])), log(sd(sample_data[, 20])), NA)
rownames <- c(rownames, colnames(sample_data[20]))
rownames(df) <- rownames

# SIMULATION
n <- 1000
sim <- data.frame(X = rep(NA, n))

# simulate the different variables

sim[, 1] <- rpois(n, df[1, 5])
sim <- cbind(sim, rpois(n, df[2, 5]),
             rpois(n, df[3, 5]),
             rpois(n, df[4, 5]),
             rpois(n, df[5, 5]),
             rpois(n, df[6, 5]),
             rpois(n, df[7, 5]),
             rbinom(n, 1, prob = df[8, 7]),
             rbinom(n, 1, prob = df[9, 7]),
             rbinom(n, 1, prob = df[10, 7]),
             rbinom(n, 1, prob = df[11, 7]),
             rbinom(n, 1, prob = df[12, 7]),
             rbinom(n, 1, prob = df[13, 7]),
             rbinom(n, 1, prob = df[14, 7]),
             rbinom(n, 1, prob = df[15, 7]),
             rpois(n, df[16, 5]),
             rpois(n, df[17, 5]),
             rpois(n, df[18, 5]),
             rbinom(n, 1, prob = df[19, 7]),
             rlnorm(n, df[20, 5], df[20, 6]),
             rlnorm(n, df[21, 5], df[21, 6]))

colnames(sim) <- rownames(df)
