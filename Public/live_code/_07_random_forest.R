
set.seed(111)

library(tidyverse)

data <- nanoparquet::read_parquet(
  "data.parquet"
)

split <- rsample::initial_split(data)

train <- rsample::training(split)

test <- rsample::testing(split)

model <- ranger::ranger(
  price ~ tenure + distance + size + district,
  train
)

model_x <- ranger::ranger(
  price ~ tenure + distance + size + district,
  train,
  replace = FALSE,
  sample.fraction = 1
) # only x is randomized

model_ols <- lm(price ~ tenure + distance + size + district,
                train)

pred <- predict(model, test)

pred_ind <- predict(model,test,predict.all = TRUE)

test$pred <- pred$predictions

test$pred_ind <- pred_ind$predictions

test$pred_ols <- predict(model_ols,test)

