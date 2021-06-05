#' Equal vector length transformation
#'
#' Equal vector length transformation
#'
#' @author Shoji F. Nakayama
#'
#' @param X data
#' @export
#'

evlt <- function(X){
  XT <- range_transform(X)
  SM <- as.matrix(apply(XT, 1, function(x) sum(x ^ 2)))
  y <- 1 / sqrt(SM)
  Y <- diag(as.vector(y))
  X11 <- Y %*% XT
  return(X11)
}
