#' Varimax
#'
#' Varimax
#'
#' @author Shoji F. Nakayama
#'
#' @param Phi A11
#' @param gamma gamma
#' @param q q
#' @param tol tolerance
#'
#' @export
#'

Varimax <- function(Phi, gamma = 1.0, q = 20, tol = 1e-6) {
  dims <- dim(Phi)
  R <- diag(dims[2])
  d <- 0
  for (i in 1:q) {
    d_old <- d
    Lambda <- Phi %*% R
    S <- La.svd(t(Phi) %*% (as.array(Lambda) ^ 3 - (gamma / dims[1]) * (Lambda %*% diag(diag(t(Lambda) %*% Lambda)))))
    R <- S$u %*% S$v
    d <- sum(S$d)
    if (d_old != 0 & d / d_old < 1 + tol) {
      break
    }
  }
  res <- Phi %*% R
  return(res)
}
