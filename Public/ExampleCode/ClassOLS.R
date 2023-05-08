
set.seed(111)

library(tidyverse)
library(mlr3verse)
library(mlr3pipelines)
library(arrow)
library(rpart.plot)

Data <- read_parquet("Example.parquet")

Task <- as_task_regr(
  Data,
  target = "Price"
)

OLS <- lrn("regr.lm")

OLS$train(Task)

OLS$model

