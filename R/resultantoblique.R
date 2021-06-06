#' Resultant oblique
#'
#' Resultant oblique
#'
#' @author Shoji F. Nakayama
#'
#' @param x A111
#' @param y F11
#' @param z O0
#' @param X X111
#'
#' @export
#'

# A111, F11, O0, X111
resultant_oblique <- function(x, y, z, X) { # Use oblique reference axes to get the resultant oblique loadings and scores
  F011 <- z %*% y
  O01<- solve(z)
  A011 <- x %*% O01
  SB <- scale_back(A011, F011, X, k)
  res <- list(A0 = SB$A0, F0 = SB$F0)
  return(res)
}
