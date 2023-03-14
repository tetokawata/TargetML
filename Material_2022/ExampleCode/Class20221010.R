library(tidyverse)
library(mlr3verse)
library(rpart.plot)

Data <- read_csv("Example.csv")

Task <- as_task_regr(Data, target = "Price")

Tree <- lrn("regr.rpart")

Tree$param_set$values$maxdepth <- 2

Tree$train(Task)

rpart.plot(Tree$model)

Tree$predict(Task)



