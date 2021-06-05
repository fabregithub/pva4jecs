#' Inspect extreme
#'
#' Inspect extreme
#'
#' @author Shoji F. Nakayama
#'
#' @param A0 A0
#' @param A111 A111
#' @param O0 O0
#' @param k Number of end-members
#'
#' @export
#'

inspect_extreme <- function(A0, A111, O0, k) {
  A0_ix1 <- matrix(0, k)
  A0_max <- matrix(0, k)
  A_copy <- A0
  A11_copy <- A111
  Z <- O0
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
