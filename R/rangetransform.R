#' Range transformation
#'
#' Range transformation
#'
#' @author Shoji F. Nakayama
#'
#' @param X data
#' @export
#'

range_transform <- function(X){
  XT <- apply(X, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  return(XT)
}

