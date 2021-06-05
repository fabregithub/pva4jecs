#' PVA analysis
#'
#' Polytopic vector analysis (PVA) function
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix, X
#' @param k number of end-members
#' @param N number of trials
#'
#' @export
#'

PVA <- function(X, k, N = 10) {
  ncols <- ncol(X)

  X111 <- row_sum(X)
  X11 <- evlt(X111)
  SVD <- La.svd(X11)

  S <- diag(SVD$d)
  A11 <- SVD$u[, 1:k] %*% S[1:k, 1:k] # loading matrix
  A11T <- t(A11)
  F111 <- solve(A11T %*% A11) %*% A11T
  F11 <- F111 %*% X11 # scores matrix

  Z <- estimate_X(X, k)

  n <- 0
  while (n < 10){
    A111 <- varimax(A11, gamma = 1.0, q = 20, tol = 1e-6)
    O0 <- A_O(A111)
    ro <- resultant_oblique(A111, F11, O0, X111, k)
    A0 <- ro$A0
    F0 <- ro$F0

    #iteration of O0
    O00 <- O0
    O0 <- inspect_extreme(A0, A111, O0, k)

    ro <- resultant_oblique(A111, F11, O0, X111, k)
    A0 <- ro$A0
    F0 <- ro$F0
    m <- 0
    while (any(!is.na(O0 - O00)) & m < 50){
      O00 <- O0
      ro <- resultant_oblique(A111, F11, O0, X111, k)
      A0 <- ro$A0
      F0 <- ro$F0
      O0 <- inspect_extreme(A0, A111, O0, k)
      m <- m + 1
    }
    O0 <- O00

    ro <- resultant_oblique(A111, F11, O0, X111, k)
    A0 <- ro$A0
    F0 <- ro$F0

    tag <- negative_A0(X, A0, k)

    if (tag != 0){
      deg <- DENEG(X, A0, k) ## step iii - vi: see function DENEG, where adjust A0 as equation 7.23
      A0 <- deg$A0
      F0 <- deg$F0
    }else{
      A0 <- A0
      F0 <- F0
    }
    tag1 <- negative_F0(X, F0, k) # step vii: test the composititon scores for positivity
    if (tag1 == 0){
      A0 <- A0
      F0 <- F0
      break
    }else{
      for (i in 1:k){
        for (j in 1:ncols){
          if (F0[i, j] < (-0.05)){
            F0[i, j] <- 0
          }else{
            F0[i, j] <- F0[i, j]
          }
        }
      }
      F0 <- row_sum(F0) #transform new F0 to row sum to 100
      F0T <- t(F0)
      A0 <- (Z %*% F0T) %*% (solve(F0 %*% F0T))
      A11 <- evlt(A0)
    }
    n <- n + 1
    print(n)
  }

  res <- list(A0, F0)
  return(res)
}
