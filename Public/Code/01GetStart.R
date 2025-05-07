library(tidyverse)

data <- read_csv("Data.csv")

lm(Tenure ~ Size + Y, data)

tree <- rpart::rpart(Tenure ~ Size + Y, data)

rpart.plot::rpart.plot(tree)
# ctr/command -> A -> Enter: run all lines
