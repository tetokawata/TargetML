set.seed(111)

library(tidyverse)
library(mlr3verse)
library(DoubleML)

Data = read_csv("Data.csv")

Y = Data$Price
D = Data$Distance
X = Data |> 
  select(
    Size,
    Tenure,
    Area
  )

X0 = model.matrix(
  ~ .^2 + poly(Size,2) + poly(Tenure,2),
  X
)

X0 = X0[,-1]

X0 |> head()

Task = double_ml_data_from_matrix(
  X = X0,
  d = D,
  y = Y
)

ModelR = DoubleMLPLR$new(
  Task,
  ml_l = lrn("regr.cv_glmnet", s= "lambda.min"), # Predict Y
  ml_m = lrn("regr.cv_glmnet", s= "lambda.min") # Predict D
)

ModelR$fit() # Estimation
ModelR$summary() # Show results
