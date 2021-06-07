#' Generate O0 from A11
#'
#' Generate O0 from A11
#'
#' @author Shoji F. Nakayama
#'
#' @param X A11
#' @param k number of end-members
#'
#' @export
#'

A_O <- function(X, k) { # get O0 from A11
  X_copy <- X
  X_ix <- matrix(0, k)
  O0 <- matrix(0, k, k)
  for (i in 1:k){
    X_ix[i] <- which.max(abs(X_copy[, i])) # get the row number of the max element in each column of A11
    O0[i,] <- X_copy[X_ix[i], ]
    X_copy <- X_copy[-X_ix[i], ]
  }
  return(O0)
}
