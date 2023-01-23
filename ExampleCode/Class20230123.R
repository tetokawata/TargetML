set.seed(1)

library(tidyverse)
library(mlr3verse)
library(DoubleML)
library(grf)

Raw <- read_csv("Example.csv")

Group <- sample(1:2, nrow(Raw), replace = TRUE)

## Estimate Nuisance

Data <- Raw[Group == 1,]
Data <- as.data.table(Data)

Task <- double_ml_data_from_data_frame(
  Data,
  x_cols = c("TradeQ", "Size", "BuildYear", "Distance"),
  d_cols = c("Reform"),
  y_col = c("Price")
)

RegLearner <- lrn("regr.ranger")

Est <- DoubleMLPLR$new(
  Task,
  RegLearner$clone(),
  RegLearner$clone()
)

Est$fit(store_predictions = TRUE)

Est$summary()

Y <- Est$data$data$Price
W <- Est$data$data$Reform
X <- select(Est$data$data,
            TradeQ,
            Size,
            BuildYear,
            Distance)
HatY <- Est$predictions$ml_l[,1,1]
HatW <- Est$predictions$ml_m[,1,1]

FitGRF <- causal_forest(
  Y = Y,
  W = W,
  X = X,
  Y.hat = HatY,
  W.hat = HatW
)

## Estimate estimand

ConfirmData <- Raw[Group == 2,]

X <- select(ConfirmData,
            TradeQ,
            Size,
            BuildYear,
            Distance)

S <- predict(FitGRF,X)

Condition <- S$predictions < median(S$predictions)

## 以下を修正

lm(Price ~ Reform + TradeQ + Size + BuildYear + Distance,
   ConfirmData[Condition,])
