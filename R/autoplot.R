#' Plot a patternogram
#'
#' @param object An output of the `patternogram()` function
#' @param ... Additional arguments to `autoplot()`
#'
#' @export
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
autoplot.patternogram = function(object, ...) {
  has_ci = all(c("ci_lower", "ci_upper") %in% colnames(object))
  has_ui = all(c("ui_lower", "ui_upper") %in% colnames(object))
  if ("group" %in% colnames(object)) {
    gg = ggplot2::ggplot(object, ggplot2::aes(.data$dist, .data$dissimilarity,
                                              color = .data$group, fill = .data$group)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Distance", y = "Dissimilarity", color = "Group", fill = "Group")
  } else {
    gg = ggplot2::ggplot(object, ggplot2::aes(.data$dist, .data$dissimilarity)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Distance", y = "Dissimilarity")
  }
  if (has_ci) {
    gg = gg +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
                           alpha = 0.3, inherit.aes = TRUE) #, fill = "#364156"
  }
  if (has_ui) {
    gg = gg +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ui_lower, ymax = .data$ui_upper),
                           alpha = 0.3, inherit.aes = TRUE) #, fill = "#D66853"
  }
  return(gg)
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
