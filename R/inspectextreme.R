#' Inspect extreme
#'
#' Inspect extreme
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix X
#' @param A0 A0
#' @param O0 O0
#' @param k Number of end-members
#'
#' @export
#'

inspect_extreme <- function(X, A0, O0, k) { # A0, A11, O0
  x <- row_sum(X)
  y <- evlt(x)
  SVD <- La.svd(y)
  S <- diag(SVD$d)
  A11 <- SVD$u[, 1:k] %*% S[1:k, 1:k] # loading matrix
  Y <- A11
  Z <- O0

  A0_ix1 <- matrix(0, k)
  A0_max <- matrix(0, k)
  A_copy <- A0
  A11_copy <- Y
  for (i in 1:k) {
    A0_max[i] <- max(A_copy)
    if (A0_max[i] > 1) {
      A0_ix1[i] <- which.max(A_copy[, i])
      Z[i, ] <- A11_copy[A0_ix1[i], ]
    }
    A_copy <- A_copy[-A0_ix1[i], ]
    A11_copy <- A11_copy[-A0_ix1[i], ]
  }
  return(Z)
}
