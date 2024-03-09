pacman::p_load(
  tidyverse,
  mlr3verse,
  mlr3pipelines,
  ranger
)

set.seed(1)

lgr::get_logger("mlr3")$set_threshold("error")

lgr::get_logger("bbotk")$set_threshold("error")

N <- 100000

Data <- tibble(
  X = runif(N,-2,2),
  D = sample(0:1,N,replace = TRUE),
  U = rnorm(N,0,10)
  ) |> 
  mutate(
    Y = X^2 + D + if_else(X >= 1,1,0) + if_else(X >= 0, 1,0) + if_else(X >= -1,1,0) + U
  )

Mutate <- po("mutate")
Mutate$param_set$values$mutation = list(
  X2 = ~ X*X
)

OLS <- Mutate %>>% lrn("regr.lm") |> as_learner()

Tree <- lrn("regr.rpart") |> lts()

Tree <- AutoTuner$new(
  learner = Tree,
  resampling = rsmp("cv",folds = 2),
  terminator = trm("evals", n_evals = 20),
  tuner = tnr("random_search")
)

Stacking <- pipeline_stacking(
  base_learners = list(
    OLS,
    Tree),
  super_learner = lrn(
    "regr.lm",
    id = "SL"),
  folds = 2,
  use_features = FALSE
) |> 
  as_learner()

Task <- as_task_regr(
  Data |> select(Y,D,X),
  "Y"
)

Stacking$train(Task)


Fig <- Data |> 
  mutate(
    D = D |> factor(),
    Pred = Stacking$predict(Task)$response,
    TrueY = Y - U) |> 
  ggplot(
    aes(
      x = X,
      y = TrueY,
      color = D
    )
  ) +
  theme_bw() +
  geom_line(
    aes(linetype = "Population")
  ) +
  geom_line(
    aes(
      y = Pred,
      linetype = "Stacking"
    )
  ) +
  ylab("E[Y|D,X]")

print(Fig)
