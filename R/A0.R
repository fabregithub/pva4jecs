#' A0
#'
#' A0
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix X
#' @param k number of end-members
#'
#' @export
#'

A_O <- function(X, k) {
  x <- row_sum(X)
  y <- evlt(x)
  SVD <- La.svd(y)
  S <- diag(SVD$d)
  A11 <- SVD$u[, 1:k] %*% S[1:k, 1:k] # loading matrix

  X_copy <- A11
  X_ix <- matrix(0, k)
  O0 <- matrix(0, k, k)
  for (i in 1:k){
    X_ix[i] <- which.max(abs(X_copy[, i])) # get the row number of the max element in each column of A11
    O0[i,] <- X_copy[X_ix[i], ]
    X_copy <- X_copy[-X_ix[i], ]
  }
  return(O0)
}
