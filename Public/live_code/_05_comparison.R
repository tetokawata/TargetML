
set.seed(111)

library(tidyverse)

data <- nanoparquet::read_parquet(
  "data.parquet"
)

estimatr::lm_robust(
  price ~ year,
  data
)


estimatr::lm_robust(
  price ~ year +
    (size + distance + tenure)^2 +
    poly(size,2) +
    poly(distance,2) +
    poly(tenure,2),
  data
)

X <- model.matrix(
  ~ year +
    (size + distance + tenure)^2 +
    poly(size,2) +
    poly(distance,2) +
    poly(tenure,2),
  data
)

X <- X[,-1]

model <- hdm::rlassoEffects(
  x = X,
  y = data$price,
  index = 1,
  method = "double selection"
)

confint(model)

model$selection.matrix

cobalt::love.plot(year ~ size + tenure + distance,
                  data = data)
# ctr + A -> ctr + Enter