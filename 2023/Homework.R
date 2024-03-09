pacman::p_load(
  tidyverse,
  arrow,
  mlr3verse,
  mlr3pipelines,
  simpr,
  DiagrammeR,
  patchwork
)

Data <- read_parquet(
  "Public/Example.parquet"
)

Homework1 <- Data |> 
  select(Price, Tenure, Size, DistanceStation)

Homework1 <- Homework1[sample(1:nrow(Homework1),500),]

Homework1 |> write_csv("Public/Homework1.csv")

Homework1 |> write_parquet("Public/Homework1.parquet")


# 駅名が増えると、やだ

