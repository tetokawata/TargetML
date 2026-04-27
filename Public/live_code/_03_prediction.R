
set.seed(111)

library(tidyverse)

data <- nanoparquet::read_parquet(
  "data.parquet"
  )

split <- rsample::initial_split(data)

train <- rsample::training(split)

test <- rsample::testing(split)

ols <- lm(price ~ size + tenure, train)

lasso <- hdm::rlasso(price ~ (size + tenure)^2 + 
              I(size^2) + I(tenure^2), 
              train) # LASSO

pred_ols <- predict(ols, test)
pred_lasso <- predict(lasso, test)

mean((test$price - pred_ols)^2)
mean((test$price - pred_lasso)^2)

mean((test$price - pred_lasso)^2)/
  mean((test$price - pred_ols)^2)
