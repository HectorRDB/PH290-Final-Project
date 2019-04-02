library(stats)
library(here)
library(texreg)
#sample_data <- read.csv(here("data", "sample_ga_data_binomial.csv"))
sample_data <- read.csv("data/sample_ga_data_binomial.csv")
head(sample_data)

train_sample_data <- sample_data[ , !(colnames(sample_data) %in% "X")]


min.model = glm(y ~ ., data=train_sample_data, family=binomial)
biggest <- formula(glm(y ~.,train_sample_data, family=binomial))
biggest
fwd.model = step(min.model, direction='forward', scope=biggest)
summary(fwd.model)
#from Hector fullmod = glm(y ~ .,family=binomial)

step_model<- step(min.model)
texreg(step_model)
