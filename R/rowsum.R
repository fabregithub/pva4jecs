#' Calculate sum of rows
#'
#' Data transform, each row sum to 100
#'
#' @author Shoji F. Nakayama
#'
#' @param x data
#' @export
#'

row_sum <- function(x){
  X_row_sum <- x / apply(x, 1, sum)
  return(X_row_sum)
}
