#' DENEG
#'
#' DENEG algorithm
#'
#' @author Shoji F. Nakayama
#'
#' @param x A0
#' @param y X_estimate
#' @param k number of end-members
#'
#' @export
#'

DENEG <- function(x, y, k){
  D <- matrix(0, nrows, k)
  A_adj <- matrix(0, nrows, k)
  sum_d <- 1
  for (i in 1:nrows){
    for (j in 1:k){
      if (x[i, j] > (-0.25) & x[i, j] < (-0.05)){
        A_adj[i, j] <- x[i, j]
      }
      else{
        A_adj[i, j] <- 0
      }
    }
  }
  for (m in 1:k){
    D[m] <- min(A_adj[, m]) # if no negative value is found in the corresponding row, D[m] equals to 0
    sum_d <- sum_d + abs(D[m])
  }

  z <- 1/sum_d
  for (i in 1:nrows){
    for (j in 1:k){
      x[i, j] <- (x[i, j] - D[j]) * z
    }
  }

  AT <- t(x)
  F1 <- solve(AT %*% x) %*% AT
  F0 <- F1 %*% y
  return(list(A0 = x, F0 = F0))
}
