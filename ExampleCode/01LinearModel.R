library(tidyverse) # command + Enter
library(hdm)

set.seed(111) # Set Seed

Data = read_csv("Data.csv")

Sample = sample(
  1:2,
  nrow(Data),
  replace = TRUE,
  prob = c(0.8,0.2)
)

glimpse(Data)

Model = lm(Price ~ Size + Tenure, Data[Sample == 1,])

ModelLong = lm(Price ~ poly(Size,2) + poly(Tenure,2),
   Data[Sample == 1,])

ModelLASSO = rlasso(
  Price ~ poly(Size,2) + poly(Tenure,2) + Size:Tenure,
  Data[Sample == 1,],
  post = FALSE)

Pred = predict(Model, Data)

PredLong = predict(ModelLong, Data)

PredLASSO = predict(ModelLASSO, Data)

1 - mean((Data$Price - Pred)[Sample == 2]^2)/var(Data$Price)
1 - mean((Data$Price - PredLong)[Sample == 2]^2)/var(Data$Price)
1 - mean((Data$Price - PredLASSO)[Sample == 2]^2)/var(Data$Price)

