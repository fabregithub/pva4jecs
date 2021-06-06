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
  nrows <- nrow(X)
  ncols <- ncol(X)

  # Data transformation, each row sum to 1
  X111 <- row_sum(X)

  # Equal vector length transformation
  X11 <- evlt(X111)
  SVD <- La.svd(X11) # Singular value decomposition
  S <- diag(SVD$d)
  A11 <- SVD$u[, 1:k] %*% S[1:k, 1:k] # loading matrix
  A11T <- t(A11)
  F111 <- solve(A11T %*% A11) %*% A11T
  F11 <- F111 %*% X11 # scores matrix
  sb <- scale_back(A11, F11, X111, k)
  X_estimate <- sb$A0 %*% sb$F0

  return(X_estimate)
}
