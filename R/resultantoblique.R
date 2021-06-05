#' Resultant oblique
#'
#' Resultant oblique
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix X
#' @param A0 A0
#' @param k Number of end-members
#'
#' @export
#'

resultant_oblique <- function(X, A0, k) { # Use oblique reference axes to get the resultant oblique loadings and scores
  ncols <- ncol(X)
  nrows <- nrow(X)
  x <- row_sum(X)
  y <- evlt(x)
  SVD <- La.svd(y)
  S <- diag(SVD$d)
  A11 <- SVD$u[, 1:k] %*% S[1:k, 1:k] # loading matrix
  A111 <- varimax(A11, gamma = 1.0, q = 20, tol = 1e-6)
  Y <- A11
  Z <- A_O(A111, k = k)

  F011 <- Z %*% Y
  O01<- solve(Z)
  A011 <- A0 %*% O01
  SB <- scale_back(X, k)
  res <- list(A0 = SB$A0, F0 = SB$F0)

  return(res)
}
