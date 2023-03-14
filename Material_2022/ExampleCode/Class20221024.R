set.seed(1) # Set Seed

library(tidyverse) # Load packages
library(mlr3verse)
library(rpart.plot)

Data <- read_csv("Example.csv") # Import Data

Evaluation <- msr("regr.rsq") # Define Evaluation

Task <- as_task_regr(Data, target = "Price") # Define Task

Group <- partition(Task, ratio = 0.8) # Split Sample

Tree <- lrn("regr.rpart") # Define Tree

CV <- rsmp("cv", folds = 2) # Define CrossValidation

Tuner <- tnr("grid_search") # Use GridSearch

Terminal <- trm("evals", n_evals = 20) # Define terminal condition

PS <- ps(cp = p_dbl(lower = 0, upper = 0.2)) # Define Search space

PruneTree <- AutoTuner$new(
  learner = Tree,
  resampling = CV,
  measure = Evaluation,
  search_space = PS,
  tuner = Tuner,
  terminator = Terminal
)

PruneTree$train(Task, Group$train)

PruneTree$predict(Task, Group$test)$score(Evaluation)

PruneTree$model

rpart.plot(PruneTree$model$learner$model)

PruneTree$model$learner$model |> 
  rpart.plot()

