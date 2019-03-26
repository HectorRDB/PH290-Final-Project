library(stats)

setwd("~/Documents/Spring 2019/PH290/Final project")
sample_data <- read.csv("sample_ga_data.csv")
head(sample_data)
  
train_sample_data <- sample_data[ , !(colnames(sample_data) %in% "m_age_anc1")]
  

min.model = lm(sample_data$m_age_anc1 ~ ., data=sample_data)
biggest <- formula(lm(sample_data$m_age_anc1~.,sample_data))
biggest
fwd.model = step(min.model, direction='forward', scope=biggest)
summary(fwd.model)


