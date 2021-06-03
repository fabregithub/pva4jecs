#' Equal vector length transformation
#'
#' Equal vector length transformation
#'
#' @author Shoji F. Nakayama
#'
#' @param x data
#' @export
#'

evlt <- function(x){
  XT <- range_transform(x)
  SM <- as.matrix(apply(XT, 1, function(x) sum(x ^ 2)))
  y <- 1 / sqrt(SM)
  Y <- diag(as.vector(y))
  X11 <- Y %*% XT
  return(X11)
}
