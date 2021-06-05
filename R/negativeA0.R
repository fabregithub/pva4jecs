#' Negative A0
#'
#' Negative A0
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix X
#' @param A0 A0
#' @param k Number of end-members
#'
#' @export
#'

negative_A0 <- function(X, A0, k) {
  nrows <- nrow(X)
  Y <- A0
  n <-0
  tag <- 0
  for (i in 1:nrows){
    for (j in 1:k){
      if (Y[i, j]> (-0.25) & Y[i, j] < (-0.05)){
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
