library(caret)
library(ggplot2)
library(lattice)
library(gbm)

library(here)
sample_data <- read.csv(here("data", "sample_ga_data.csv"))

set.seed(147)
inTraining <- createDataPartition(sample_data$m_age_anc1, p = .75, list = FALSE)
training <- sample_data[ inTraining,]
testing  <- sample_data[-inTraining,]


gbmFit1 <- train(m_age_anc1 ~ ., data = training,
                 method = "gbm",
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

gbmImp <- varImp(gbmFit1, scale = TRUE)
gbmImp


sample_data[order(-sample_data$parity),]
