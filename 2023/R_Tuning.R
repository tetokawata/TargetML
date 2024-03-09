
set.seed(111)

pacman::p_load(
  tidyverse,
  mlr3verse,
  mlr3mbo,
  mlr3hyperband,
  arrow
)

Raw <- read_parquet("Public/Example.parquet")

Task <- as_task_regr(
  Raw,
  "Price"
)

Tree <- lrn("regr.rpart") |> 
  lts()


Terminal20 <- trm(
  "evals",
  n_evals = 20)


Terminal100 <- trm(
  "evals",
  n_evals = 100)

TreeMbo20 <- AutoTuner$new(
  tuner = tnr("mbo"),
  learner = Tree,
  resampling = rsmp("holdout"),
  terminator = Terminal20
)

TreeMbo20$id <- "MBO20"


TreeMbo100 <- AutoTuner$new(
  tuner = tnr("mbo"),
  learner = Tree,
  resampling = rsmp("holdout"),
  terminator = Terminal100
)

TreeMbo100$id <- "MBO100"

TreeRandom20 <- AutoTuner$new(
  tuner = tnr("random_search"),
  learner = Tree,
  resampling = rsmp("holdout"),
  terminator = Terminal20
)

TreeRandom20$id <- "Random20"


TreeRandom100 <- AutoTuner$new(
  tuner = tnr("random_search"),
  learner = Tree,
  resampling = rsmp("holdout"),
  terminator = Terminal100
)

TreeRandom100$id <- "Random100"


TreeGird20 <- AutoTuner$new(
  tuner = tnr("grid_search"),
  learner = Tree,
  resampling = rsmp("holdout"),
  terminator = Terminal20
)

TreeGird20$id <- "Gird"


TreeGird100 <- AutoTuner$new(
  tuner = tnr("grid_search"),
  learner = Tree,
  resampling = rsmp("holdout"),
  terminator = Terminal100
)

TreeGird100$id <- "Gird"

Design <- benchmark_grid(
  tasks = Task,
  learners = list(
    TreeRandom20,
    TreeGird20,
    TreeMbo20,
    TreeRandom100,
    TreeGird100,
    TreeMbo100
    ),
  resamplings = rsmp(
    "holdout",
    ratio = 0.8) 
)

BenchMark <- benchmark(Design)

BenchMark$aggregate(msr("regr.rsq"))
