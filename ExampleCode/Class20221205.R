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

Learner <- lrn("regr.ranger")

Estimation <- DoubleMLPLR$new(
  Task,
  ml_l = Learner$clone(),
  ml_m = Learner$clone()
)

Estimation$fit()

Estimation$summary()
Estimation$confint()

