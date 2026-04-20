library(tidyverse)

data <- nanoparquet::read_parquet(
  "data.parquet"
)

lm(price ~ size + tenure, data)

lm(
  price ~ (size + tenure)^2 + 
    I(size^2) + I(tenure^2), 
  data) # OLS

hdm::rlasso(price ~ (size + tenure)^2 + 
              I(size^2) + I(tenure^2), 
            data) # LASSO
