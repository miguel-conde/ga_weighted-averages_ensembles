## gaWARE - genetic algorithms Weighted Averaged Regression Ensembles
require(GA)
require(caret)


### Fitness function
fitness_gaWARE <- function(w, y, y_hat, 
                           maximize = TRUE, perfFunc = RMSE) {
  
  if (ncol(y_hat) != nrow(as.matrix(w)))
    stop("y_hat number of cols and w number of rows must be equal")
  
  y_hat_ens <- as.matrix(y_hat) %*% w
  
  out <- perfFunc(y, y_hat_ens)
  
  if (maximize == FALSE) {
    out <- -out
  }
  
  out
}

myMAE <- function(y, y_hat) {
  return(mean(abs(y - y_hat)))
}