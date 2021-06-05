#' X estimate
#'
#' Obtain X estimate
#'
#' @author Shoji F. Nakayama
#'
#' @param x SVD data = SVD
#' @param y transformed data = X111
#' @param z original data = X
#' @param k number of end-members
#'
#' @export
#'

estimate_X <- function(x, y, z, k) {
  S <- diag(x$d)
  A11 <- x$u[, 1:k] %*% S[1:k, 1:k] # loading matrix
  X11 <- evlt(y)

  A11T <- t(A11)
  F111 <- solve(A11T %*% A11) %*% A11T
  F11 <- F111 %*% X11 # scores matrix

  # Transform A11, F11 to original data
  sb <- scale_back(A11, F11, y, ncols = ncol(z), k = k, nrows = nrow(z))
  X_estimate <- sb$A0 %*% sb$F0
  A0_initial <- sb$A0

  return(X_estimate)
}
