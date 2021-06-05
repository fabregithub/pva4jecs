#' Resultant oblique
#'
#' Resultant oblique
#'
#' @author Shoji F. Nakayama
#'
#' @param A111 A111
#' @param F11 F11
#' @param O0 O0
#' @param X111 X111
#' @param k Number of end-members
#'
#' @export
#'

# A111, F11, O0, X111
resultant_oblique <- function(A111, F11, O0, X111, k) { # Use oblique reference axes to get the resultant oblique loadings and scores
  F011 <- O0 %*% F11
  O01<- solve(O0)
  A011 <- A111 %*% O01
  SB <- scale_back(A011, F011, X111, k)
  res <- list(A0 = SB$A0, F0 = SB$F0)

  return(res)
}
