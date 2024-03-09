library(DoubleML)
library(mlr3verse)
library(mlr3pipelines)
library(arrow)
library(tidyverse)

Data <- read_parquet("Example.parquet")

RegOLS <- lrn("regr.lm")
RegRF <- lrn("regr.ranger")

RegStack <- pipeline_stacking(
  list(RegOLS,RegRF),
  lrn("regr.lm", id = "Stacking")
)

ClassifLogit <- lrn(
  "classif.log_reg",
  predict_type = "prob")

ClassifRF <- lrn(
  "classif.ranger",
  predict_type = "prob")

ClassifStack <- pipeline_stacking(
  list(ClassifLogit,ClassifRF),
  lrn("classif.log_reg", id = "Stacking")
  )

RegStack <- as_learner(RegStack)
ClassifStack <- as_learner(ClassifStack)

Y <- Data$Price
D <- Data$After
X <- select(Data,Tenure,Size)
X <- as.data.table(X)

Task <- double_ml_data_from_matrix(
  X = X,
  d = D,
  y = Y
)

AIPW <- DoubleMLIRM$new(
  Task,
  RegStack,
  ClassifStack
)

AIPW$fit(
  store_predictions = TRUE
)

AIPW
