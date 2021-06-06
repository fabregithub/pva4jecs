#' Negative A0
#'
#' Negative A0
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix X
#' @param Y A0
#' @param k number of end-members
#'
#' @export
#'

negative_A0 <- function(X, Y, k){
  nrows <- nrow(X)

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
