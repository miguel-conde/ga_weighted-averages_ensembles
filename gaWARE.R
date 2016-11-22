## gaWARE - genetic algorithms Weighted Averaged Regression Ensembles
require(GA)
require(Metrics)
require(caret)


### Fitness function
#' Title
#'
#' @param w 
#' @param y 
#' @param y_hat 
#' @param maximize 
#' @param perfFunc 
#'
#' @return
#' @export
#'
#' @examples
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

## Cross Validation
#' Title
#'
#' @param type 
#' @param numFolds 
#' @param seed 
#' @param y 
#' @param y_hat 
#' @param maximize 
#' @param perfFunc 
#' @param min 
#' @param max 
#' @param popsize 
#' @param maxiter 
#' @param names 
#' @param monitor 
#'
#' @return
#' @export
#'
#' @examples
cvOpt_gaWARE <- function(type = "cv", numFolds = 10, seed = 345,
                         y, y_hat, maximize = TRUE, perfFunc = RMSE,
                         min, max, popsize = 50, maxiter = 100, names,
                         monitor = NULL) {
  if (type == "cv") {
    set.seed(seed)
    idxFolds <- createFolds(y, k = numFolds, returnTrain = TRUE)
    
    ga <- lapply(idxFolds, function(fold_i) {
      GA <- ga(type = "real-valued",
               fitness = function(x, ...) fitness_gaWARE(x, ...),
               ## '...' for fitness_gaWARE()
               y = y[fold_i], y_hat = y_hat[fold_i,],
               maximize = maximize, perfFunc = perfFunc,
               ##
               min = min, max = max, popSize = popsize,
               maxiter = maxiter, names = c("w1", "w2", "w3"),
               seed = seed,
               monitor = monitor)
      return(GA)
    })
    
    ws <- do.call(rbind, lapply(ga, function(x) return(x@solution)))
    fv <- do.call(rbind, lapply(ga, function(x) return(x@fitnessValue)))
    rownames(ws) <- rownames(fv)
    colnames(fv) <- "Fitness function value"
    
    ws_out           <- list()
    
    ws_out$GA        <- ga
    
    ws_out$ws        <- ws
    ws_out$mn        <- apply(ws, 2, mean, na.rm = TRUE)
    ws_out$sd        <- apply(ws, 2, sd, na.rm = TRUE)
    ws_out$t_test    <- apply(ws, 2, t.test, na.rm = TRUE)
    
    if (maximize == FALSE) {
      fv <- -fv 
    }
    
    ws_out$fv        <- fv
    ws_out$fv_mn     <- apply(fv, 2, mean, na.rm = TRUE)
    ws_out$fv_sd     <- apply(fv, 2, sd, na.rm = TRUE)
    ws_out$fv_t_test <- apply(fv, 2, t.test, na.rm = TRUE)
    
    return(ws_out)
  }
  else {
    GA <- ga(type = "real-valued",
             fitness = function(x, ...) fitness_gaWARE(x, ...),
             ## '...' for fitness_gaWARE()
             y = y, y_hat = y_hat,
             maximize = FALSE, perfFunc = myMAE,
             ##
             min = minVector, max = maxVector, popSize = 50,
             maxiter = 100, names = c("w1", "w2", "w3"),
             seed = 789,
             monitor = NULL)
    return(GA)
  }
}

#' Title
#'
#' @param y 
#' @param y_hat 
#'
#' @return
#' @export
#'
#' @examples
myMAE <- function(y, y_hat) {
  return(mean(abs(y - y_hat)))
}