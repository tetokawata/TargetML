set.seed(1)

library(tidyverse)
library(mlr3verse)
library(DoubleML)

Data <- read_csv("Example.csv")

Data <- as.data.table(Data)

Task <- double_ml_data_from_data_frame(
  Data,
  x_cols = c("TradeQ", "Size", "BuildYear", "Distance"),
  d_cols = c("Reform"),
  y_col = c("Price")
)

RegLearner <- lrn("regr.ranger")

ProbLearner <- lrn("classif.ranger")

Est <- DoubleMLIRM$new(
  Task,
  RegLearner$clone(),
  ProbLearner$clone(),
  trimming_threshold = 0.1
)

Est$fit(store_predictions = TRUE)

Est

hist(Est$predictions$ml_m)
