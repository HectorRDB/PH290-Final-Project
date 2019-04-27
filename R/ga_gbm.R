library(caret)
library(pROC)
library("gbm")

##varimp script
ga_data_x <- read.csv("Desktop/binomial.csv", header = TRUE)
ga_data <- ga_data_x[ , !(colnames(ga_data_x) %in% "X")]
inTraining <- createDataPartition(ga_data$y, p = .75, list = FALSE)
training <- ga_data[ inTraining,]
testing  <- ga_data[-inTraining,]

gbmFit1 <- train(y ~ ., data = training,
                 method = "gbm",
                 verbose = FALSE)
gbmFit1

imp <- varImp(gbmFit1)

imp


### function
gbm <- function(data, col) {
  inTraining <- createDataPartition(col , p = .75, list = FALSE)
  training <- data[ inTraining,]
  testing  <- data[-inTraining,]

  gbmFit1 <- train( y ~ ., data = training,
                    method = "gbm",
                    verbose = FALSE)
  gbmFit1

  imp <- varImp(gbmFit1)

  imp}


##prediction accuracy
plot_gbm(gbmFit1)

predictions <- predict(gbmFit1, testing, type = 'raw')
predictions

outcometbl <- cbind(predictions, testing[,"dhc"], testing[,"y"] )
outcometbl

colnames(outcometbl) <- c("pred", "dhc", "y")
outcometbl[order(outcometbl[,"pred"], decreasing = T),]
