set.seed(1) # Set Seed

library(tidyverse) # Load packages
library(mlr3verse)
library(rpart.plot)

Data <- read_csv("Example.csv") # Import Data

Evaluation <- msr("regr.rsq") # Define Evaluation

Task <- as_task_regr(Data, target = "Price") # Define Task

Group <- partition(Task, ratio = 0.8) # Split Sample

Tree <- lrn("regr.rpart") # Define Tree

Bagging <- lrn("regr.ranger") # Define Bagging
Bagging$param_set$values$mtry <- 5

OLS <- lrn("regr.lm") # Define OLS

Tree$train(Task, Group$train) # Fit
Bagging$train(Task, Group$train)
OLS$train(Task,Group$train)

PredictOLS <- OLS$predict(Task,Group$test) # Predict
PredictTree <- Tree$predict(Task,Group$test)
PredictBagging <- Bagging$predict(Task,Group$test)

PredictOLS$score(Evaluation) # Evaluation
PredictTree$score(Evaluation)
PredictBagging$score(Evaluation)
