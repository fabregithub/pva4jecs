#' Resultant oblique
#'
#' Resultant oblique
#'
#' @author Shoji F. Nakayama
#'
#' @param x A111
#' @param y F11
#' @param z O0
#' @param Z X111
#' @param X original data matrix X
#' @param k number of end-members
#'
#' @export
#'

# A111, F11, O0, X111
resultant_oblique <- function(x, y, z, Z, X, k) { # Use oblique reference axes to get the resultant oblique loadings and scores
  F011 <- z %*% y
  O01<- solve(z)
  A011 <- x %*% O01
  SB <- scale_back(X, A011, F011, Z, k)
  res <- list(A0 = SB$A0, F0 = SB$F0)
  return(res)
}
