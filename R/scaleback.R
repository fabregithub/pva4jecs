#' Transform A11, F11 to original data
#'
#' Transform A11, F11 to original data
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix X
#' @param A11 A11
#' @param F11 F11
#' @param X111 X111
#' @param k Number of end-members
#'
#' @export
#'

scale_back <- function(X, A11, F11, X111, k) {
  ncols <- ncol(X)
  nrows <- nrow(X)

  xmin <- matrix(0, ncols)
  xmax <- matrix(0, ncols)

  for (j in 1:ncols){
    xmin[j] <- min(X111[,j])
    xmax[j] <- max(X111[,j])
  }

  sum1 <- sum(xmin)
  K <- 100
  sk <- matrix(0,k)
  F1 <- matrix(0, k, ncols)
  F0 <- matrix(0, k, ncols)
  A1 <- matrix(0, nrows, k)
  A <- matrix(0, nrows, k)
  sumA1 <- matrix(0, nrows,1)

  for (i in 1:k){
    m <- 0
    for (j in 1:ncols){
      m <- m + F11[i,j]*(xmax[j]-xmin[j])
    }
    sk[i] <- (K-sum1)/m
  }

  for (i in 1:k){
    for (j in 1:ncols){
      F1[i,j] <- sk[i]*F11[i,j]
      F0[i,j] <- F1[i, j]*(xmax[j]-xmin[j]) + xmin[j]
    }
  }

  for (i in 1:nrows){
    for (j in 1:k){
      A1[i,j] <- A11[i, j]/sk[j]
    }
    sumA1[i] <- sum(A1[i,])
  }

  for (i in 1:nrows){
    for (j in 1:k){
      A[i,j] <- A1[i,j]/sumA1[i,1]
    }
  }

  ans <- list(A0 = A, F0 = F0, sk = sk)

  return(ans)
}
