#' CD plot
#'
#' Creating coefficient of determination (CD) plot to determine the best number
#' of end-members (`k`).
#'
#' @param X Original data matrix.
#' @param k Number of end-members.
#'
#' @return A ggplot object.
#'
#' @author Shoji F. Nakayama
#'
#' @importFrom rlang .data
#'
#' @export
CD_plot <- function(X, k) {
  x <- row_sum(X)
  y <- estimate_X(X, k)

  cn <- colnames(x)

  observed <- as.data.frame(x) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Names",
      values_to = "Observed"
    )

  colnames(y) <- cn

  predicted <- as.data.frame(y) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Names.2",
      values_to = "Predicted"
    )

  cd_data <- cbind(observed, predicted)
  cd_data$Names <- factor(cd_data$Names, levels = cn)

  ggplot2::ggplot(
    data = cd_data,
    ggplot2::aes(x = .data$Observed, y = .data$Predicted)
  ) +
    ggplot2::theme_bw() +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.5
    ) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(ggplot2::vars(.data$Names), scales = "free") +
    ggplot2::labs(
      title = paste("CD plot for k =", k),
      x = "Measured values (transformed)",
      y = "Back calculated values"
    ) +
    ggpubr::stat_cor(
      ggplot2::aes(label = ggplot2::after_stat(.data$rr.label)),
      digits = 3
    )
}
