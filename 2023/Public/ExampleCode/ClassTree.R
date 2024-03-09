
set.seed(111)

library(tidyverse)
library(mlr3verse)
library(arrow)
library(rpart.plot)

Data <- read_parquet("Example.parquet")

Tree <- lrn("regr.rpart")

Task <- as_task_regr(Data, "Price")

Tree$train(Task)

Tree$model

rpart.plot(Tree$model)

# Estimate shallow tree

ShallowTree <- lrn("regr.rpart") # Define algorithm

ShallowTree$param_set$values$maxdepth <- 2 # Set max depth
ShallowTree$param_set$values$cp <- 0 # Set cp

ShallowTree$train(Task)

rpart.plot(ShallowTree$model)
