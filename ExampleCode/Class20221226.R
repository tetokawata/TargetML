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

PsuY <- Est$psi_b[,1,1]

mean(PsuY)

Z1 <- Data$BuildYear |> scale()
Z1 <- Z1[,1]
Z2 <- Data$Size |> scale()
Z2 <- Z2[,1]
estimatr::lm_robust(PsuY ~ Z1 + Z2)

## Partialling out

Task <- double_ml_data_from_data_frame(
  Data,
  x_cols = c("TradeQ","Size","BuildYear","Distance"),
  d_cols = c("Reform"),
  y_col = c("Price")
)

PLR <- DoubleMLPLR$new(Task,
                       RegLearner$clone(),
                       RegLearner$clone()
                       )
PLR$fit(store_predictions = TRUE)

OhtY <- Data$Price - PLR$predictions$ml_l[,1,1]
OhtD <- Data$Reform - PLR$predictions$ml_m[,1,1]
Int1 <- Z1*OhtD
Int2 <- Z2*OhtD

estimatr::lm_robust(OhtY ~ 0 + OhtD + Int1 + Int2)
