#' @export
#' @importFrom ggplot2 autoplot
autoplot.patternogram = function(plot_data, ...) {
  if ("target" %in% colnames(plot_data)){
    ggplot(plot_data, aes(.data$dist, .data$dissimilarity, color = .data$target)) +
      geom_point() +
      labs(x = "Distance", y = "Dissimilarity", color = "Target")
  } else {
    ggplot(plot_data, aes(.data$dist, .data$dissimilarity)) +
      geom_point() +
      labs(x = "Distance", y = "Dissimilarity")
  }
}

#' @export
#' @importFrom graphics plot
plot.patternogram = function(x, ...) {
  print(autoplot(x, ...))
}
