library(mlr3verse)
library(DALEX)
library(DALEXtra)
library(tidyverse)

Data <- read_csv("Example.csv")

Model <- lrn("regr.ranger")

Task <- as_task_regr(
  Data,
  "Price"
)

Model$train(Task)

ModelExplain <- explain_mlr3(Model,
  data = select(Data, -Price),
  y = Data$Price
)

plot(model_profile(ModelExplain,
  variable = "Distance",
  type = "partial"
))

plot(predict_parts(ModelExplain,
  new_observation = Data[1, ],
  type = "shap",
  B = 2
))
