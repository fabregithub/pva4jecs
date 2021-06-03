#' Negative A0
#'
#' Negative A0
#'
#' @author Shoji F. Nakayama
#'
#' @param X data
#' @param nrows Number of rows in the original matrix X
#' @param k Number of end-members
#'
#' @export
#'

negative_A0 <- function(X, nrows, k){
  n <-0
  tag <- 0
  for (i in 1:nrows){
    for (j in 1:k){
      if (X[i, j]> (-0.25) & X[i, j] < (-0.05)){
        n <- n + 1
      }
      else{
        n <- n
      }
    }
  }
  if (n == 0){
    return(tag)
  }
  else{
    return(tag + 1)
  }
}