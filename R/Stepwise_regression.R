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
# forward stepwise regression
fwd.model = step(min.model, direction='forward', scope=biggest)
summary(fwd.model)

# order coefficients based on p-value 
ranked_by_pvalue <- summary(fwd.model)$coefficients[, c(1,4)][order(summary(fwd.model)$coefficients[,4]),]
ranked_by_pvalue

# variable selection based on AIC 
step_model<- step(min.model)
texreg(step_model)
