library(tidyverse)

data <- nanoparquet::read_parquet("data.parquet")

Y <- data$price
D <- data$reform
X <- select(data,size,tenure,distance,district)

X <- model.matrix(~ 0 + ., X)

model <- grf::causal_forest(
  Y = Y,
  X = X,
  W = D
)

hist(model$predictions)

data$pred_tau <- model$predictions[,1]
