#' Rescale Values in a Patternogram
#'
#' It rescales the dissimilarity values in a patternogram object to a specified range.
#' This allows for easier comparison and interpretation of various patternograms.
#'
#' @param x A patternogram object
#' @param method Scaling method, currently only "minmax" is implemented
#' @param by Optional grouping variable (e.g., "id" or "group") to apply scaling within groups
#' @param ... Additional arguments for future extensions
#'
#' @return A patternogram object with rescaled dissimilarity values (0 to 1 for "minmax" method)
#'
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' r = terra::rast(system.file("ex/elev.tif", package = "terra"))
#' pr = patternogram(r)
#' pr
#' pr_scaled = rescale_patternogram(pr)
#' pr_scaled
#' plot(pr_scaled)
rescale_patternogram = function(x, method = c("minmax"), by = "id", ...) {
  UseMethod("rescale_patternogram")
}

#' @export
rescale_patternogram.patternogram = function(x, method = c("minmax"), by = "id", ...) {
  method = match.arg(method)

  if (!is.null(by) && by %in% names(x)) {
    x = dplyr::group_by(x, .data[[by]])
  }

  x = switch(method,
              minmax = dplyr::mutate(x,
                                     dissimilarity = ( .data$dissimilarity - min(.data$dissimilarity)) /
                                       (max(.data$dissimilarity) - min(.data$dissimilarity))
              )
  )

  if ("id" %in% names(x)) {
    x = dplyr::ungroup(x)
  }

  class(x) = c("patternogram", setdiff(class(x), "patternogram"))
  x
}
