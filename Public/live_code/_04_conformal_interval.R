
set.seed(111)

library(tidyverse)

data <- nanoparquet::read_parquet(
  "data.parquet"
)

split <- rsample::initial_split(data)

train <- rsample::training(split)

test <- rsample::testing(split)

ols <- lm(price ~ size + tenure + district, train)

pred <- predict(ols, test)

score <- abs(test$price - pred)

#hist(score)

cutoff <- quantile(
  score, 
  probs = 0.9*(nrow(test) + 1)/nrow(test))

test$upper <- (pred + cutoff)
test$lower <- (pred - cutoff)
