
set.seed(111)

library(tidyverse)
library(SuperLearner)

data <- nanoparquet::read_parquet(
  "data.parquet"
)

split <- rsample::initial_split(data)

train <- rsample::training(split)

test <- rsample::testing(split)

Y <- train$price
X <- select(train,tenure,size)

Y_test <- test$price
X_test <- select(test,tenure,size)

model <- SuperLearner(
  Y = Y,
  X = X,
  SL.library = c(
    "SL.lm",
    "SL.ranger",
    "SL.glmnet",
    "SL.mean",
    "SL.nnet")
)

model

pred <- predict(model, X_test)

test$pred_SL <- pred$pred[,1]
test$pred_OLS <- pred$library.predict[,1]

