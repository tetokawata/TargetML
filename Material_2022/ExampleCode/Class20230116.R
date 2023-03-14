set.seed(1)

library(tidyverse)
library(mlr3verse)
library(DoubleML)
library(grf)

Data <- read_csv("Example.csv")

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

## 予測

predict(FitGRF, 
        tibble(
          TradeQ = 1,
          Size = 100,
          BuildYear = 1970,
          Distance = 1
        ),
        estimate.variance = TRUE
        )

Pred <- predict(FitGRF,
                estimate.variance = TRUE)

predict(FitGRF,
        X[1,],
        estimate.variance = TRUE)

Result <- cbind(X,Pred)
Result <- mutate(Result,
                 SD = sqrt(variance.estimates))

average_treatment_effect(FitGRF,
                         subset = Result$predictions <= median(Result$predictions))

average_treatment_effect(FitGRF,
                         subset = Result$predictions >= median(Result$predictions))
