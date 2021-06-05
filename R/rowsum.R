#' Calculate sum of rows
#'
#' Data transform, each row sum to 100
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix X
#' @export
#'

row_sum <- function(X){
  X_row_sum <- X / apply(X, 1, sum)
  return(X_row_sum)
}
