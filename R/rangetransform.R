#' Range transformation
#'
#' Range transformation
#'
#' @author Shoji F. Nakayama
#'
#' @param x data
#' @export
#'

range_transform <- function(x){
  XT <- apply(x, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  return(XT)
}

