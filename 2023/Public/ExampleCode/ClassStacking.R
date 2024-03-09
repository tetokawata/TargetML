set.seed(111)

library(tidyverse)
library(mlr3verse)
library(mlr3pipelines)
library(arrow)

Data <- read_parquet("Example.parquet")

Task <- as_task_regr(
  Data,
  target = "Price"
)

OLS <- lrn("regr.lm")
RF <- lrn("regr.ranger")

Base <- list(OLS, RF)

Aggre <- lrn("regr.lm")

Aggre$id <- "Aggregation"

Stacking <- pipeline_stacking(
  base_learners = Base,
  super_learner = Aggre,
  folds = 2,
  use_features = FALSE
)

Stacking$plot()

Stacking <- as_learner(Stacking)

Test <- Stacking$clone()$train(Task)

Test$model$Aggregation$model

Design <- benchmark_grid(
  Task,
  learners = list(
    OLS,
    RF,
    Stacking
  ),
  resamplings = rsmp("cv", folds = 2)
)

BenchMark <- benchmark(Design)

BenchMark$aggregate(msr("regr.rsq"))

