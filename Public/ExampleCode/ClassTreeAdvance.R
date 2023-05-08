
set.seed(111)

library(tidyverse)
library(mlr3verse)
library(arrow)
library(rpart.plot)

Data <- read_parquet("Example.parquet")

Tree <- lrn("regr.rpart")

OLS <- lrn("regr.lm")

TreeOptimal <- lts(lrn("regr.rpart"))

TreeOptimal <- AutoTuner$new(
  learner = TreeOptimal,
  resampling = rsmp("cv", folds = 2),
  terminator = trm("evals", n_evals = 20),
  tuner = tnr("random_search")
)

Task <- as_task_regr(Data, "Price")

Bench <- benchmark_grid(
  Task,
  list(Tree, OLS, TreeOptimal),
  rsmp("cv")
)

Result <- benchmark(Bench)

Result$aggregate(msr("regr.rsq"))
