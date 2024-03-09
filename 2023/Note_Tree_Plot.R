pacman::p_load(
  tidyverse,
  rpart,
  ggplot2,
  ggparty,
  rpart.plot,
  patchwork
)

Data <- tibble(
  X = seq(-10,10,1),
  Y = runif(length(X))
)

depth <- 2

MakeFigure <- function(Data,depth){
  Fit <- rpart(
    Y ~ X,
    Data,
    control = rpart.control(
      cp = 0,
      maxdepth = depth,
      minsplit = 1,
      minbucket = 1
    )
  ) |> 
    as.party()
  
  Pred <- predict(Fit)
  
  FigTree <- Fit |> 
    ggparty() +
    geom_edge() +
    geom_edge_label() +
    geom_node_info()
  R2 <- 1 - mean((Data$Y - Pred)^2)/var(Data$Y)
  FigPlot <- Data |> 
    mutate(
      Pred
    ) |> 
    ggplot(
      aes(
        x = X,
        y = Y
      )
    ) +
    theme_bw() +
    geom_point() +
    geom_line(
      aes(
        y = Pred
      )
    ) +
    xlab(paste(round(R2,3)))
  
  Fig <- FigPlot + FigTree
  
  return(Fig)
}

MakeFigure(Data,6)


