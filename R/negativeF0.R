#' Negative F0
#'
#' Negative F0
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix X
#' @param F0 F0
#' @param k Number of end-members
#'
#' @export
#'

negative_F0 <- function(X, F0, k){
  ncols <- ncol(X)
  Y <- F0
  n <- 0
  tag1 <- 0
  for (i in 1:k){
    for (j in 1:ncols){
      if (Y[i, j]< (-0.05)){
        n <- n + 1
      }else{
        n <- n
      }
    }
  }
  if (n == 0){
    return(tag1)
  }else{
    return(tag1 + 1)
  }
}
