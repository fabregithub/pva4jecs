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
  XT <- apply(X, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  SM <- as.matrix(apply(XT, 1, function(x) sum(x ^ 2)))
  y <- 1 / sqrt(SM)
  Y <- diag(as.vector(y))
  X11 <- Y %*% XT
  return(X11)
}
