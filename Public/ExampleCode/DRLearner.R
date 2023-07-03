library(tidyverse)
library(mlr3verse)
library(mlr3extralearners)
library(arrow)

Data <- read_parquet("Example.parquet")

Y <- Data$Price
D <- Data$Reform
X <- select(Data, Tenure, Size)

Learner <- lrn("regr.bart")

Group <- sample(1:3, length(Y), replace = TRUE)

# Nuisance

## D

TaskD <- as_task_regr(
  mutate(X[Group == 1,], D = D[Group == 1]),
  "D"
)

FitD <- Learner$clone()$train(TaskD)

TestD <- as_task_regr(
  mutate(X, D = D),
  "D"
)

PredD <- FitD$predict(TestD)

hist(PredD$response)


## Y1

TaskY1 <- as_task_regr(
  mutate(X[Group == 1 & D == 1,], Y = Y[Group == 1 & D == 1]),
  "Y"
)

FitY1 <- Learner$clone()$train(TaskY1)

TestY <- as_task_regr(
  mutate(X, Y = Y),
  "Y"
)

PredY1 <- FitY1$predict(TestY)

## Y0

TaskY0 <- as_task_regr(
  mutate(X[Group == 1 & D == 0,], Y = Y[Group == 1 & D == 0]),
  "Y"
)

FitY0 <- Learner$clone()$train(TaskY0)

TestY <- as_task_regr(
  mutate(X, Y = Y),
  "Y"
)

PredY0 <- FitY0$predict(TestY)

## M

M <- PredY1$response - PredY0$response + 
  if_else(D == 1, Y - PredY1$response,0)/PredD$response -
  if_else(D == 0, Y - PredY0$response,0)/(1-PredD$response)

## DR learner

TaskM <- as_task_regr(
  mutate(X[Group == 2,], M = M[Group == 2]),
  "M"
)

FitM <- Learner$clone()$train(TaskM)

TestM <- as_task_regr(
  mutate(X, M = M),
  "M"
)

PredM <- FitM$predict(TestM)

hist(PredM$response)

# TreatmentEffectRisk

q <- 0.5

EstRisk <- function(q){
  Q <- quantile(PredM$response[Group == 2], q)
  RiskM <- Q + if_else(PredM$response[Group == 3] <= Q,
                       M[Group == 3] - Q,
                       0)/q
  Fit <- estimatr::lm_robust(RiskM ~ 1)
  Result <- estimatr::tidy(Fit)
  Result$q <- q
  return(Result)
}

Result <- map_dfr(seq(0.1,1,0.1), EstRisk)

ggplot(
  Result,
  aes(
    x = q,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )
) +
  geom_line() +
  geom_pointrange()
