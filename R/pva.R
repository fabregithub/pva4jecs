#' PVA analysis
#'
#' Polytopic vector analysis (PVA) function
#'
#' @author Shoji F. Nakayama
#'
#' @param k number of end-members
#' @param x X11
#' @param y transformed data = X111
#' @param z original data = X
#' @param X X_estimate
#' @param N number of trials
#'
#' @export
#'

PVA <- function(k, x, y, z, X, N = 10) {
  SVD <- La.svd(x)
  S <- diag(SVD$d)
  A11 <- SVD$u[, 1:k] %*% S[1:k, 1:k] # loading matrix
  A11T <- t(A11)
  F111 <- solve(A11T %*% A11) %*% A11T
  F11 <- F111 %*% x # scores matrix

  n <- 0
  while (n < N){
    A111 <- varimax(A11, gamma = 1.0, q = 20, tol = 1e-6)
    O0 <- A_O(A111, k = k)
    ro <- resultant_oblique(A111, F11, O0, y, k = k, ncols = ncol(z), nrows = nrow(z))
    A0 <- ro$A0
    F0 <- ro$F0

    #iteration of O0
    O00 <- O0
    O0 <- inspect_extreme(A0, A111, O0, k = k)

    ro <- resultant_oblique(A111, F11, O0, y, k = k, ncols = ncol(z), nrows = nrow(z))
    A0 <- ro$A0
    F0 <- ro$F0
    m <- 0
    while (any(!is.na(O0 - O00)) & m < 50){
      O00 <- O0
      ro <- resultant_oblique(A111, F11, O0, y, k = k, ncols = ncol(z), nrows = nrow(z))
      A0 <- ro$A0
      F0 <- ro$F0
      O0 <- inspect_extreme(A0, A111, O0, k = k)
      m <- m + 1
    }
    O0 <- O00

    ro <- resultant_oblique(A111, F11, O0, y, k = k, ncols = ncol(z), nrows = nrow(z))
    A0 <- ro$A0
    F0 <- ro$F0

    tag <- negative_A0(A0, nrows = nrow(z), k = k)

    if (tag != 0){
      deg <- DENEG(A0, X, k = k, nrows = nrow(z)) ## step iii - vi: see function DENEG, where adjust A0 as equation 7.23
      A0 <- deg$A0
      F0 <- deg$F0
    }else{
      A0 <- A0
      F0 <- F0
    }
    tag1 <- negative_F0(F0, ncols = ncol(z), k = k) # step vii: test the composition scores for positivity
    if (tag1 == 0){
      A0 <- A0
      F0 <- F0
      break
    }else{
      for (i in 1:k){
        for (j in 1:ncol(z)){
          if (F0[i, j] < (-0.05)){
            F0[i, j] <- 0
          }else{
            F0[i, j] <- F0[i, j]
          }
        }
      }
      F0 <- row_sum(F0) #transform new F0 to row sum to 100
      F0T <- t(F0)
      A0 <- (X %*% F0T) %*% (solve(F0 %*% F0T))
      A11 <- evlt(A0)
    }
    n <- n + 1
    print(n)
  }

  res <- list(A0, F0)
  return(res)
}
