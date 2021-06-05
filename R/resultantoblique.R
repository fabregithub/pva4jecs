#' Resultant oblique
#'
#' Resultant oblique
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix X
#' @param A0 A0
#' @param O0 O0
#' @param A111 A111
#' @param k Number of end-members
#'
#' @export
#'

# A111, F11, O0, X111
resultant_oblique <- function(X, A0, O0, A111, k) { # Use oblique reference axes to get the resultant oblique loadings and scores
  ncols <- ncol(X)
  nrows <- nrow(X)

  x <- row_sum(X)
  y <- evlt(x)
  SVD <- La.svd(y)
  S <- diag(SVD$d)
  A11 <- SVD$u[, 1:k] %*% S[1:k, 1:k] # loading matrix
  A11T <- t(A11)
  F111 <- solve(A11T %*% A11) %*% A11T
  F11 <- F111 %*% y # scores matrix

  F011 <- O0 %*% F11
  O01<- solve(O0)
  A011 <- A111 %*% O01
  SB <- scale_back(A011, F011, X, k)
  res <- list(A0 = SB$A0, F0 = SB$F0)

  return(res)
}
