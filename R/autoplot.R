#' @export
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
autoplot.patternogram = function(plot_data, ...) {
  if ("target" %in% colnames(plot_data)){
    ggplot2::ggplot(plot_data, ggplot2::aes(.data$dist, .data$dissimilarity, color = .data$target)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Distance", y = "Dissimilarity", color = "Target")
  } else {
    ggplot2::ggplot(plot_data, ggplot2::aes(.data$dist, .data$dissimilarity)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Distance", y = "Dissimilarity")
  }
}

#' @export
#' @importFrom graphics plot
plot.patternogram = function(x, ...) {
  print(autoplot(x, ...))
}
