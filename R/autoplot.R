#' Plot a patternogram
#'
#' @param object An output of the `patternogram()` function
#' @param point_size Size of the points in the plot
#' @param ... Additional arguments to `autoplot()`
#'
#' @export
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
autoplot.patternogram = function(object, point_size = 2, ...) {
  has_ci = all(c("ci_lower", "ci_upper") %in% colnames(object))
  has_ui = all(c("ui_lower", "ui_upper") %in% colnames(object))

  if (all(c("id", "group") %in% colnames(object))) {
    color_var   = "group"
    shape_var   = "id"
    legend_title_color = "Group"
    legend_title_shape = "Patternogram"
  } else if ("group" %in% colnames(object)) {
    color_var   = "group"
    shape_var   = NULL
    legend_title_color = "Group"
    legend_title_shape = NULL
  } else if ("id" %in% colnames(object)) {
    color_var   = NULL
    shape_var   = "id"
    legend_title_color = NULL
    legend_title_shape = "Patternogram"
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
        alpha = 0.3, inherit.aes = TRUE
      )
  }
  if (has_ui) {
    gg = gg +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$ui_lower, ymax = .data$ui_upper),
        alpha = 0.3, inherit.aes = TRUE
      )
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
