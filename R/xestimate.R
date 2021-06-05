#' X estimate
#'
#' Obtain X estimate
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix
#' @param k number of end-members
#'
#' @export
#'

estimate_X <- function(X, k) {
  x <- row_sum(X)
  y <- evlt(x)
  SVD <- La.svd(y)

  S <- diag(SVD$d)
  A11 <- SVD$u[, 1:k] %*% S[1:k, 1:k] # loading matrix
  y <- evlt(x)

  A11T <- t(A11)
  F111 <- solve(A11T %*% A11) %*% A11T
  F11 <- F111 %*% y # scores matrix

  # Transform A11, F11 to original data
  sb <- scale_back(A11, F11, X, k)
  X_estimate <- sb$A0 %*% sb$F0
  A0_initial <- sb$A0

  return(X_estimate)
}
