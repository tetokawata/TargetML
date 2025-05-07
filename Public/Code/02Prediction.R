set.seed(111)

library(tidyverse)

data <- read_csv("Data.csv")

group <- sample(0:1,
  nrow(data),
  replace = TRUE,
  prob = c(0.8, 0.2)
)

train <- data[group == 0, ]
test <- data[group == 1, ]

model_1 <- lm(Y ~ Size, train)

model_2 <- lm(Y ~ ., train)

predict_1 <- predict(model_1, test)

predict_2 <- predict(model_2, test)

plot(predict_1, predict_2)

1 - mean((test$Y - predict_1)^2) / var(test$Y)
1 - mean((test$Y - predict_2)^2) / var(test$Y)
# ctr + A -> ctr + Enter
