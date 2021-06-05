#' CD plot
#'
#' Creating coeﬀicient of determination (CD) plot to determine the best number of end-members (k)
#'
#' @author Shoji F. Nakayama
#'
#' @param X oritinal data matrix X
#' @param k number of end-members
#'
#' @export
#'

CD_plot <-function(X, k) {
  x <- row_sum(X)
  y <- estimate_X(X, k)

  cn <- colnames(x)
  Observed <- as.data.frame(x) %>% gather(cn, key = 'Names', value = 'Observed')
  colnames(y) <- cn
  Predicted <- as.data.frame(y) %>% gather(cn, key = 'Names.2', value = 'Predicted')
  CD_data <- cbind(Observed, Predicted)
  CD_data$Names <- CD_data$Names %>% factor(levels = cn)

  cd <- ggplot(data = CD_data, aes(x = Observed, y = Predicted)) + theme_bw() +
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, colour = 'red', size = 0.5) +
    geom_point() +
    facet_wrap(~Names, scales = 'free') +
    labs(title = paste('CD plot for k =', k, sep = ' '),
         x = 'Measured values (transformed)',
         y = 'Back calculated values') +
    stat_cor(aes(label = ..rr.label..), digits = 3, colour = 'red')

  return(cd)
}

