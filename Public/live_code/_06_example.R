
set.seed(111)

library(tidyverse)

data <- nanoparquet::read_parquet(
  "data.parquet"
)

model_Y <- hdm::rlasso(
  price ~ size + tenure + distance + district,
  data)

model_D <- hdm::rlasso(
  year ~ size + tenure + distance + district,
  data)

select <- model_Y$index + model_D$index

select <- as.logical(select)

X <- model.matrix(
  ~ size + tenure + distance + district,
  data)
X <- X[,-1]

estimatr::lm_robust(data$price ~ data$year + X) # OLS

estimatr::lm_robust(
  data$price ~ data$year + X[,model_Y$index]
  ) # 非推奨

estimatr::lm_robust(
  data$price ~ data$year + X[,select]
) # 2重選択

