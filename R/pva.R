#' PVA analysis
#'
#' Fit a polytopic vector analysis (PVA) model.
#'
#' @author Shoji F. Nakayama
#'
#' @param X Original data matrix.
#' @param k Number of end-members.
#' @param N Maximum number of trials.
#' @param verbose Logical. If `TRUE`, print iteration progress messages.
#'
#' @return A named list with `A0` and `F0` matrices.
#' @export
#'
PVA <- function(X, k, N = 10, verbose = FALSE) {
  X <- as.matrix(X)

  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 1) {
    stop("`k` must be a single positive number.", call. = FALSE)
  }
  if (k > min(nrow(X), ncol(X))) {
    stop("`k` must be no larger than min(nrow(X), ncol(X)).", call. = FALSE)
  }
  if (!is.numeric(N) || length(N) != 1 || is.na(N) || N < 0) {
    stop("`N` must be a single non-negative number.", call. = FALSE)
  }

  k <- as.integer(k)
  N <- as.integer(N)
  ncols <- ncol(X)

  has_changed <- function(new, old) {
    !isTRUE(all.equal(new, old, check.attributes = FALSE))
  }

  # Data transformation, each row sum to 1.
  X111 <- row_sum(X)

  # Equal vector length transformation.
  X11 <- evlt(X111)
  SVD <- La.svd(X11)

  S <- diag(SVD$d)
  A11 <- SVD$u[, 1:k, drop = FALSE] %*% S[1:k, 1:k, drop = FALSE]

  A11T <- t(A11)
  F111 <- solve(A11T %*% A11) %*% A11T
  F11 <- F111 %*% X11

  sb <- scale_back(X, A11, F11, X111, k)
  A0 <- sb$A0
  F0 <- sb$F0
  X_estimate <- A0 %*% F0

  n <- 0
  while (n < N) {
    A111 <- Varimax(A11, gamma = 1.0, q = 20, tol = 1e-6)
    O0 <- A_O(A111, k)
    ro <- resultant_oblique(A111, F11, O0, X111, X, k)
    A0 <- ro$A0
    F0 <- ro$F0

    # Iterate O0 until the selected extremes are stable or the safety cap is hit.
    O00 <- O0
    O0 <- inspect_extreme(A0, A111, O0, k)
    ro <- resultant_oblique(A111, F11, O0, X111, X, k)
    A0 <- ro$A0
    F0 <- ro$F0

    m <- 0
    while (has_changed(O0, O00) && m < 50) {
      O00 <- O0
      ro <- resultant_oblique(A111, F11, O0, X111, X, k)
      A0 <- ro$A0
      F0 <- ro$F0
      O0 <- inspect_extreme(A0, A111, O0, k)
      m <- m + 1
    }

    O0 <- O00
    ro <- resultant_oblique(A111, F11, O0, X111, X, k)
    A0 <- ro$A0
    F0 <- ro$F0

    tag <- negative_A0(X, A0, k)
    if (tag != 0) {
      deg <- DENEG(X, A0, X_estimate, k)
      A0 <- deg$A0
      F0 <- deg$F0
    }

    tag1 <- negative_F0(X, F0, k)
    if (tag1 == 0) {
      break
    }

    for (i in seq_len(k)) {
      for (j in seq_len(ncols)) {
        if (F0[i, j] < (-0.05)) {
          F0[i, j] <- 0
        }
      }
    }

    F0 <- row_sum(F0)
    F0T <- t(F0)
    A0 <- (X_estimate %*% F0T) %*% solve(F0 %*% F0T)
    A11 <- evlt(A0)

    n <- n + 1
    if (isTRUE(verbose)) {
      message("PVA iteration: ", n)
    }
  }

  list(A0 = A0, F0 = F0)
}
