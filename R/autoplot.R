#' Plot a patternogram
#'
#' @param object An output of the `patternogram()` function
#' @param ... Additional arguments to `autoplot()`
#'
#' @export
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
autoplot.patternogram = function(object, ...) {
  if ("target" %in% colnames(object)){
    ggplot2::ggplot(object, ggplot2::aes(.data$dist, .data$dissimilarity, color = .data$target)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Distance", y = "Dissimilarity", color = "Target")
  } else {
    ggplot2::ggplot(object, ggplot2::aes(.data$dist, .data$dissimilarity)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Distance", y = "Dissimilarity")
  }
}
#' Plot a patternogram
#'
#' @param x An output of the `patternogram()` function
#' @param ... Additional arguments to `autoplot()`
#'
#' @export
#' @importFrom graphics plot
plot.patternogram = function(x, ...) {
  print(autoplot(x, ...))
}
