set.seed(1) # Set Seed

library(tidyverse) # Load packages
library(mlr3verse)
library(rpart.plot)

Data <- read_csv("Example.csv") # Import Data

OLS <- lrn("regr.lm")
Tree <- lrn("regr.rpart")
Tree <- lts(Tree)

CV <- rsmp("cv", folds = 2)
R2 <- msr("regr.rsq")
Tuner <- tnr("random_search")
Terminal <- trm("evals", n_evals = 20)

Task <- as_task_regr(Data,"Price")
Group <- partition(Task, ratio = 0.8)

Tree <- AutoTuner$new(
  learner = Tree,
  resampling = CV,
  measure = R2,
  tuner = Tuner,
  Terminal
)

Design <- benchmark_grid(
  tasks = Task$clone()$filter(Group$train),
  learners = list(OLS,Tree),
  resamplings = CV
)

Bench <- benchmark(Design)

future::plan("multisession") # 並列処理

Bench$aggregate(R2) # 交差検証
