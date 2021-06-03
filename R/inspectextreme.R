#' Inspect extreme
#'
#' Inspect extreme
#'
#' @author Shoji F. Nakayama
#'
#' @param X A0
#' @param Y A11
#' @param Z O0
#' @param k Number of end-members
#'
#' @export
#'

inspect_extreme <- function(X, Y, Z, k) {# A0, A11, O0
  A0_ix1 <- matrix(0, k)
  A0_max <- matrix(0, k)
  A_copy <- X
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
