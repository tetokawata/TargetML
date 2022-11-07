set.seed(1)

library(tidyverse)
library(mlr3verse)
library(mlr3pipelines)

Data <- read_csv("Example.csv")
Task <- as_task_regr(Data, "Price")
Group <- partition(Task, ratio = 0.8)

# OLS/RandomForest/LASSOのStacking

LASSO <- AutoTuner$new(
  learner = lts(lrn("regr.glmnet")),
  resampling = rsmp("cv", folds = 2),
  measure = msr("regr.rsq"),
  terminator = trm("evals", n_evals = 20),
  tuner = tnr("random_search")
)

# OLS/RandomForest/LASSOのStacking
PipeStack <- pipeline_stacking(
  base_learners =
    list(
      lrn("regr.lm"),
      lrn("regr.ranger"),
      LASSO$clone()
    ),
  super_learner = lrn("regr.lm", id = "sum"),
  use_features = FALSE
)

PipeStack$plot()
Stack <- as_learner(PipeStack)

Stack$train(Task, Group$train)

Stack$predict(Task, Group$test)$score(msr("regr.rsq"))
Stack$model$sum$model

LASSO$train(Task, Group$train)

LASSO$predict(Task, Group$test)$score(msr("regr.rsq"))
