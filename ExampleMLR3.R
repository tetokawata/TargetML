
library(tidyverse)
library(mlr3verse)
library(mlr3pipelines)
library(ranger)

Data = read_csv("Data.csv") |> 
  filter(
    TradeQ == 2
  )

Task = as_task_regr(
  Data |> select(Size,Tenure,Price),
  target = "Price"
)

OLS = lrn("regr.lm")
LASSO = lrn("regr.cv_glmnet",
            s = "lambda.min")
RF = lrn("regr.ranger")

Stack = pipeline_stacking(
  base_learners = list(
    OLS$clone(),
    LASSO$clone(),
    RF$clone()
  ),
  super_learner = lrn("regr.lm", id = "OLS"),
  folds = 2,
  use_features = FALSE
  ) |> 
  as_learner()
Group = partition(Task,ratio = 0.8)

OLS$train(Task, Group$train)$predict(Task,Group$test)$score()
LASSO$train(Task, Group$train)$predict(Task,Group$test)$score()
RF$train(Task, Group$train)$predict(Task,Group$test)$score()
Stack$train(Task, Group$train)$predict(Task,Group$test)$score()

