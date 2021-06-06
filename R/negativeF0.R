#' Negative F0
#'
#' Negative F0
#'
#' @author Shoji F. Nakayama
#'
#' @param X F0
#' @param k number of end-members
#'
#' @export
#'

negative_F0 <- function(X, k){
  n <- 0
  tag1 <- 0
  for (i in 1:k){
    for (j in 1:ncols){
      if (X[i, j]< (-0.05)){
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
