library(caret)
library(pROC)
library("gbm")

gbm <- function(data) {
  if ("X" %in% colnames(data)) {
    j <- which(colnames(data) == "X")
    data <- data[, -j]
  }
  data$y <- as.factor(data$y)
  gbmFit <- train( y ~ ., data = data,
                    method = "gbm",
                    verbose = FALSE)
  return(varImp(gbmFit))
}

