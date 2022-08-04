library(tidyverse)
library(tree)
library(randomForest)

train_data <- read.csv2(
  "bank-additional.csv",
  header = TRUE
)
full_data <- read.csv2(
  "bank-additional-full.csv",
  header = TRUE
)
train_data <- as_tibble(train_data)
full_data <- as_tibble(full_data)
#convert some char columns to factors
train_data$loan <- as_factor(train_data$loan)
train_data$housing <- as_factor(train_data$housing)
train_data$education <- as_factor(train_data$education)
train_data$default <- as_factor(train_data$default)
train_data$job <- as_factor(train_data$job)
train_data$y <- as_factor(train_data$y)
train_data$poutcome <- as_factor(train_data$poutcome)
train_data$marital <- as_factor(train_data$marital)
train_data$day_of_week <- as_factor(train_data$day_of_week)

full_data$loan <- as_factor(full_data$loan)
full_data$housing <- as_factor(full_data$housing)
full_data$education <- as_factor(full_data$education)
full_data$default <- as_factor(full_data$default)
full_data$job <- as_factor(full_data$job)
full_data$y <- as_factor(full_data$y)
full_data$poutcome <- as_factor(full_data$poutcome)
full_data$marital <- as_factor(full_data$marital)
full_data$day_of_week <- as_factor(full_data$day_of_week)

#convert some columns to double
train_data$euribor3m <- as.double(train_data$euribor3m)
full_data$euribor3m <- as.double(full_data$euribor3m)
train_data$cons.conf.idx <- as.double(train_data$cons.conf.idx)
full_data$cons.conf.idx <- as.double(full_data$cons.conf.idx)

#using logistic regression to fit model
logistic1 <- glm(
  y~age+education+euribor3m,
  data = train_data,
  family = binomial
)
result_logi <- predict(logistic1, full_data, type = "response")
result_logi[result_logi<0.5] <- 0
result_logi[result_logi>=0.5] <- 1

#calculate the error rate
contingency <- table(full_data$y, result_logi)
error_rate_logi <- 
  (contingency[1,2]+contingency[2,1])/nrow(full_data)

#use decision tree method
tree1 <- tree(
  y~age+euribor3m,
  data = train_data
)
result_tree <- predict(
  tree1,
  newdata = full_data, 
  type = "class"
)
contingency <- table(full_data$y, result_tree)
error_rate_tree <- 
  (contingency[1,2]+contingency[2,1])/nrow(full_data)

#use random forest
random_forest1 <- randomForest(
  y~age+euribor3m,
  data = train_data,
  importance = TRUE
)
result_random_forest <- predict(
  random_forest1,
  newdata = full_data
)
contingency <- table(full_data$y, result_random_forest)
error_rate_random_forest <- 
  (contingency[1,2]+contingency[2,1])/nrow(full_data)

