#Questions:
# 1. data size - the same?
# 2. same mean, standard deviation etc.? 
# 3. binomial or continuous?

library(stats)
library(here)
library(texreg)
library(simstudy)
#sample_data <- read.csv(here("data", "sample_ga_data_binomial.csv"))
sample_data <- read.csv("data/sample_ga_data_binomial.csv")
head(sample_data)
summary(sample_data)
n <- 1000


library(fitdistrplus)
library(logspline)


disc_dist <- function(x){
  
  #descdist(x, boot = 1000, discrete = TRUE)
  # looks like neg binomial or poisson
  fit_pois<- fitdist(x, "pois")
  fit_nbinom <- fitdist(x, "nbinom")
  fit_norm <- fitdist(x, "norm")
  #plot(fit_pois, fit_nbinom, fit_norm)
  aic <- c("Poisson" = fit_pois$aic, "Negative Binomial" = fit_nbinom$aic, "Normal" = fit_norm$aic)
  return(aic)
}

df <- data.frame("Poisson" = rep(NA,21), "Negative Binomial" = rep(NA,21), "Normal" = rep(NA,21),"LogNormal" = rep(NA,21), "mean" = rep(NA,21), "sd" = rep(NA,21), "pr" = rep(NA,21))
rownames <- c()
for (i in c(3:18)){
  fit <- disc_dist(sample_data[,i])
  df[i-2,] <-c(fit, NA, mean(sample_data[,i]), sd(sample_data[,i]), NA)
  rownames <- c(rownames, colnames(sample_data[i]))
}

for (i in c(21:23)){
  fit <- disc_dist(sample_data[,i])
  df[i-4,] <-c(fit, NA, mean(sample_data[,i]), sd(sample_data[,i]), NA)
  #df <- rbind(df, fit)
  rownames <- c(rownames, colnames(sample_data[i]))
}

# to get the probability for the binary variables 
for (i in c(8:15)){
  y <- sum(sample_data[,i + 2])
  df[i, 7] <- y/384
}

df[19,7] <- sum(sample_data[,23])/384
#df <- cbind(df, rep(NA, 19))
#df <- cbind(df, rep(0, 19))
#df <- cbind(df, rep(0, 19))
#colnames(df) <- c("Poisson", "Negative Binomial", "Normal", "LogNormal", "mean", "sd")
df


# for the two continuous variables head_cir and length_b
x <- sample_data[,19]
descdist(x, boot = 1000, discrete = FALSE)
# looks like lognormal

fit_19 <- fitdist(x, "lnorm")
df[20,] <- c(NA, NA, NA, fit_19$aic, log(mean(sample_data[,19])),log(sd(sample_data[,19])), NA)
rownames <- c(rownames, colnames(sample_data[19]))

# for sample length_b
x <- sample_data[,20]
descdist(x, boot = 1000, discrete = FALSE)
# looks like lognormal

fit_20 <- fitdist(x, "lnorm")
df[21,] <- c(NA, NA, NA, fit_20$aic, log(mean(sample_data[,20])),log(sd(sample_data[,20])), NA)
rownames <- c(rownames,colnames(sample_data[20]))
rownames(df) <- rownames

df




# SIMULATION
n <- 1000
sim <- data.frame(X = rep(NA, n))

#simulate the different variables 

sim[,1] <- rpois(n, df[1,5])
sim <- cbind(sim, rpois(n, df[2,5]))
sim <- cbind(sim, rpois(n, df[3,5]))
sim <- cbind(sim, rpois(n, df[4,5]))
sim <- cbind(sim, rpois(n, df[5,5]))
sim <- cbind(sim, rpois(n, df[6,5]))
sim <- cbind(sim, rpois(n, df[7,5]))
sim <- cbind(sim, rbinom(n,1,prob = df[8,7]))
sim <- cbind(sim, rbinom(n,1,prob = df[9,7]))
sim <- cbind(sim,rbinom(n,1,prob = df[10,7]))
sim <- cbind(sim, rbinom(n,1,prob = df[11,7]))
sim <- cbind(sim, rbinom(n,1,prob = df[12,7]))
sim <- cbind(sim, rbinom(n,1,prob = df[13,7]))
sim <- cbind(sim, rbinom(n,1,prob = df[14,7]))
sim <- cbind(sim, rbinom(n,1, prob = df[15,7]))
sim <- cbind(sim, rpois(n, df[16,5]))
sim <- cbind(sim, rpois(n, df[17,5]))
sim <- cbind(sim, rpois(n, df[18,5]))
sim <- cbind(sim, rbinom(n,1, prob =df[19,7]))
sim <- cbind(sim, rlnorm(n, df[20,5], df[20,6]) )
sim <- cbind(sim, rlnorm(n, df[21,5], df[21,6]))

colnames(sim) <- rownames(df)
sim




# for lognormal
#rlnorm(n, meanlog =, sdlog = )


# for poission
#rpois(n, lambda =)

# for normal
#rnorm(n, mean =, sd =)

# for binomial
#rbinom(n, size = , prob =)


# with simstudy - not so practical
#def <- defData(varname = names(sample_data)[3], formula = mean(sample_data[,3]), dist = "poisson", id = #"idnum")
#def <- defData(def, varname = names(sample_data)[4], formula = mean(sample_data[,4]), dist = "negBinomial"#, id = "idnum")
#def <- defData(def, varname = names(sample_data)[5], formula = mean(sample_data[,5]), dist = "poisson", id #= "idnum")
#def <- defData(def, varname = names(sample_data)[6], formula = mean(sample_data[,6]), dist = "negBinomial"#, id = "idnum")
#def <- defData(def, varname = names(sample_data)[7], formula = mean(sample_data[,7]), dist = "negBinomial"#, id = "idnum")
#def <- defData(def, varname = names(sample_data)[8], formula = mean(sample_data[,8]), dist = "negBinomial"#, id = "idnum")
#def <- defData(def, varname = names(sample_data)[9], formula = mean(sample_data[,9]), dist = "binary", id #= "idnum")
#def <- defData(def, varname = names(sample_data)[10], formula = mean(sample_data[,10]), dist = "binary", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[11], formula = mean(sample_data[,11]), dist = "binary", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[12], formula = mean(sample_data[,12]), dist = "binary", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[13], formula = mean(sample_data[,13]), dist = "binary", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[14], formula = mean(sample_data[,14]), dist = "binary", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[15], dist = "binary", formula = mean(sample_data[,15]), #id = "identity")
#def <- defData(def, varname = names(sample_data)[16], formula = mean(sample_data[,16]), dist = "binary", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[17], formula = mean(sample_data[,17]), dist = "binary", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[18], formula = mean(sample_data[,18]), dist = "normal", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[19], formula = mean(sample_data[,19]), dist = "gamma", #variance = var(sample_data[,19]), id = "idnum")
#def <- defData(def, varname = names(sample_data)[20], formula = mean(sample_data[,20]), dist = "gamma", #variance = var(sample_data[,20]), id = "idnum")
#def <- defData(def, varname = names(sample_data)[21], formula = mean(sample_data[,21]), dist = "normal", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[22], formula = mean(sample_data[,22]), dist = "normal", #id = "idnum")
#def <- defData(def, varname = names(sample_data)[23], formula = mean(sample_data[,23]), dist = "binary", #id = "idnum")


#n <- 1000
#dt <- genData(n, def)
#dt
