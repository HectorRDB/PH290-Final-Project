library(stats)
library(here)
sample_data <- read.csv(here("data", "sample_ga_data.csv"))
head(sample_data)

train_sample_data <- sample_data[ , !(colnames(sample_data) %in% "m_age_anc1")]


min.model = lm(sample_data$m_age_anc1 ~ ., data=sample_data)
biggest <- formula(lm(sample_data$m_age_anc1~.,sample_data))
biggest
fwd.model = step(min.model, direction='forward', scope=biggest)
summary(fwd.model)


