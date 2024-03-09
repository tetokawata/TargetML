
pacman::p_load(
  tidyverse,
  arrow
)

Data <- arrow::read_parquet(
  "Public/Example.parquet"
)


yb <- Data[1:1000,1:2] 

xb <- Data[1:1000,] |> select(DistanceStation,Size) |> as.matrix()

n <- nrow(yb) 

y <- as.matrix(yb[1:(n-1),],nrow=(n-1)) 

x <- as.matrix(xb[1:(n-1),],nrow=(n-1)) 

y0 <- as.matrix(yb[n,],nrow=1) 

x0 <- as.matrix(xb[n,],nrow=1) 

fun=lm_multi()

full <- conformal.multidim.full(
  x,
  y,
  x0, 
  fun$train, 
  fun$predict,
  num.grid.pts.dim = 300) 

plot_multidim(full) 
