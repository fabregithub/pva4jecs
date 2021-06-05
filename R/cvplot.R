#' Cumulative variance plot
#'
#' Plotting cumulative variance plot to determine the best number of end-members (k)
#'
#' @author Shoji F. Nakayama
#'
#' @param X original data matrix, X
#'
#' @export
#'

CV_plot <-function(X) {
  x <- row_sum(X)
  y <- evlt(x)
  z <- La.svd(y)

  s_sumi <- 0
  s_sum <- sum(z$d)
  l <- length(z$d) - 1
  s_cumu <- matrix(0, l)
  for (i in 1:l) {
    s_sumi <- s_sumi + z$d[i]
    s_cumu[i] <- s_sumi * 100 / s_sum
  }

  # Cumulative variance plot
  k_candidate <- 1:l
  cumvar <- data.frame(k_candidate = k_candidate, s_cumu = round(s_cumu,2))
  cp <- ggplot(data = cumvar, aes(x = k_candidate, y = s_cumu)) + theme_bw() +
    geom_path() +
    geom_point(shape = 21, size = 3) +
    geom_text(aes(label = s_cumu), nudge_y = 2) +
    labs(title = 'Cumulative variance',
         x = 'Number of end-members',
         y = 'Cummulative percentage variance') +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_x_continuous(breaks = seq(1, l, 1))

  return(cp)
}

