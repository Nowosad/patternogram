#' Plot a patternogram
#'
#' Provides both `autoplot()` and `plot()` methods for objects of class `patternogram`. These functions visualize the dissimilarity relation to distance, with optional grouping, identifiers, and confidence/uncertainty intervals if present.
#'
#' The plotting behavior adapts automatically depending on the columns available in the object:
#'
#' * If `group` is present, curves/points are colored by group.
#' * If `id` is present, curves/points have different shapes by patternogram.
#' * If both `group` and `id` are present, both aesthetics are used.
#' * If `ci_lower` / `ci_upper` or `ui_lower` / `ui_upper` are present,
#'   shaded ribbons are drawn to show confidence or uncertainty intervals.
#'
#' @param object,x An object of class `patternogram`, usually created by [patternogram()].
#' @param point_size Numeric; size of the points (default 2).
#' @param ... Additional arguments passed to [ggplot2::autoplot()] (for  `autoplot.patternogram()`) or forwarded to it (for `plot.patternogram()`).
#'
#' @return For `autoplot.patternogram()`, a [ggplot2::ggplot] object that can be further modified. For `plot.patternogram()`, the plot is drawn directly and invisibly returns the `ggplot` object.
#'
#' @export
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
autoplot.patternogram = function(object, point_size = 2, ...) {
  has_ci = all(c("ci_lower", "ci_upper") %in% colnames(object))
  has_ui = all(c("ui_lower", "ui_upper") %in% colnames(object))

  if (all(c("id", "group") %in% colnames(object))) {
    color_var   = "group"; legend_title_color = "Group"
    shape_var   = "id"; legend_title_shape = "Patternogram"
  } else if ("group" %in% colnames(object)) {
    color_var   = "group"; legend_title_color = "Group"
    shape_var   = NULL; legend_title_shape = NULL
  } else if ("id" %in% colnames(object)) {
    color_var   = NULL; legend_title_color = NULL
    shape_var   = "id"; legend_title_shape = "Patternogram"
  } else {
    color_var   = NULL
    shape_var   = NULL
  }

  if (!is.null(color_var) || !is.null(shape_var)) {
    gg = ggplot2::ggplot(
      object,
      ggplot2::aes(
        x = .data$dist,
        y = .data$dissimilarity,
        color = if (!is.null(color_var)) .data[[color_var]] else NULL,
        fill  = if (!is.null(color_var)) .data[[color_var]] else NULL,
        shape = if (!is.null(shape_var)) .data[[shape_var]] else NULL
      )
    ) +
      ggplot2::geom_point(size = point_size) +
      ggplot2::labs(
        x = "Distance",
        y = "Dissimilarity",
        color = legend_title_color,
        fill  = legend_title_color,
        shape = legend_title_shape
      ) +
      ggplot2::scale_x_continuous(limits = c(0, NA))
    # if (!is.null(shape_var)) {
    #   gg = gg + ggplot2::scale_shape_manual(
    #     values = c(21, 22, 23, 24, 25)
    #   )
    # }
  } else {
    gg = ggplot2::ggplot(object,
                         ggplot2::aes(x = .data$dist, y = .data$dissimilarity)) +
      ggplot2::geom_point(size = point_size) +
      ggplot2::labs(x = "Distance", y = "Dissimilarity") +
      ggplot2::scale_x_continuous(limits = c(0, NA))
  }

  if (has_ci) {
    gg = gg +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
        alpha = 0.2, inherit.aes = TRUE
      )
  }
  if (has_ui) {
    gg = gg +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$ui_lower, ymax = .data$ui_upper),
        alpha = 0.2, inherit.aes = TRUE
      )
  }
  return(gg)
}

#' @rdname autoplot.patternogram
#' @export
#' @method plot patternogram
#' @importFrom graphics plot
plot.patternogram = function(x, ...) {
  print(autoplot(x, ...))
}
