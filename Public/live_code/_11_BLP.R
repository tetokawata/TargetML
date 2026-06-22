library(tidyverse)

data <- nanoparquet::read_parquet("data.parquet")

Y <- data$price
D <- data$reform
X <- select(data,size,tenure)

model <- ddml::ddml_plm(
  y = Y,
  D = D,
  X = data.matrix(X),
  learners = list(
    list(fun = ddml::ols),
    list(fun = ddml::mdl_ranger)
  ),
  shortstack = TRUE,
  sample_folds = 2
)

data$Y_res <- model$ols_fit$model$y_r

data$D_res <- model$ols_fit$model$D_r

estimatr::lm_robust(
  Y_res ~ D_res + 
    scale(tenure):D_res + scale(size):D_res, 
  data)
