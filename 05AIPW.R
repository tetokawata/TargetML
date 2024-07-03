set.seed(111)

library(tidyverse)
library(mlr3verse)
library(DoubleML)
library(mlr3pipelines)

Data = read_csv("Data.csv")

Y = Data$Price
D = Data$Reform

X = Data |> 
  select(
    Tenure,
    District
  ) |> 
  mutate(
    District = factor(
      District,
      labels = "Dist"
    )
  )

# Predict Y

OLS = lrn("regr.lm")
RF = lrn("regr.ranger")

Stack = pipeline_stacking(
  base_learners = list(
    OLS,
    RF
  ),
  super_learner = lrn("regr.lm",id = "OLS"),
  folds = 2,
  use_features = FALSE
) |> 
  as_learner()

# Predict D

Logit = lrn("classif.log_reg",
            predict_type = "prob")
RF_Prob = lrn("classif.ranger",
              predict_type = "prob")

Stack_Prob = pipeline_stacking(
  base_learners = list(
    Logit,
    RF_Prob
  ),
  super_learner = lrn(
    "classif.log_reg",
    id = "Logit"),
  folds = 2,
  use_features = FALSE
) |> 
  as_learner()

## Estimation

Task = double_ml_data_from_matrix(
  y = Y,
  d = D,
  X = X
)

AIPW = DoubleMLIRM$new(
  Task,
  Stack,
  Stack_Prob,
  n_folds = 2
)

AIPW$fit(store_predictions = TRUE) # Estimate Average treatment effect

AIPW

PsuY = AIPW$psi_b[,1,1] # Get psude outcome

estimatr::lm_robust(
  PsuY ~ ., X
) # Estimate BLP
