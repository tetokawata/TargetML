SimData <- function(i,n){
  set.seed(i)
  Result <- tibble(
    X = runif(n,-5,5),
    Z = runif(n,-5,5),
    U = runif(n,-20,20)
  ) |> 
    mutate(
      D = if_else(
        X >= -2 & X <= 2,
        sample(0:1,n,replace = TRUE, prob = c(2/10,8/10)),
        sample(0:1,n,replace = TRUE, prob = c(8/10,2/10))
      )
    ) |> 
    mutate(
      Y = 5*D + X^2 + Z^2 + U
    )
  return(Result)
}
