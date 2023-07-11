
library(tidyverse)
library(DoubleML)
library(mlr3verse)
library(mlr3pipelines)
library(arrow)

Data <- read_parquet("Example.parquet")

Y <- Data$Price
D <- Data$After
X <- select(Data,
            -Price,
            -After,
            -DistanceStation)
ID <- Data$DistanceStation # Clustering variable

Task <- double_ml_data_from_matrix(
  y = Y,
  d = D,
  X = X,
  cluster_vars = ID # Clustering variable
)
Task

Fit <- DoubleMLPLR$new(
  Task,
  lrn("regr.lm"),
  lrn("regr.lm"),
  n_rep = 10 # 繰り返し回数
)

Fit$fit()

Fit

DoubleMLIRM$new(
  Task,
  lrn("regr.lm"),
  lrn("classif.log_reg"),
  trimming_threshold = 0.05
)

Fit$all_coef # 全ての推定値
Fit$all_se # 全ての標準誤差
