#' Create a Patternogram or a Patternogram Cloud
#'
#' It creates a patternogram or a patternogram cloud. The function takes a raster object of class `SpatRaster` or a point vector object of class `sf` as input, and calculates the dissimilarity between values of pairs of points given the distances between them. The output of this function is a tibble of the patternogram class that can be visualized with the `plot()` and `autoplot()` functions.
#'
#' @param x A raster object of class SpatRaster (terra) or a point vector object of class sf (sf)
#' @param cutoff Spatial distance up to which point pairs are included in patternogram estimates;
#'   by default: a square root of the raster area
#' @param width The width of subsequent distance intervals for which data point pairs are grouped for patternogram estimates
#' @param dist_fun Distance measure used. This function uses the `philentropy::distance()` function (run `philentropy::getDistMethods()` to find possible distance measures)
#' @param sample_size Only used when `x` is raster. Proportion of the cells inside of each region to be used in calculations. Value between 0 and 1. It is also possible to specify an integer larger than 1, in which case the specified number of cells of each region will be used in calculations.
#' @param cloud Logical; if TRUE, calculate the patternogram cloud
#' @param target Additional argument allowing to calculate separate estimates for different categories or ranges of values
#' @param ... Additional arguments for `base::cut()`
#'
#' @return A tibble of the patternogram class with columns, such as (a) np - the number of point pairs in this estimate, (b) dist - the middle of the distance interval used for each estimate, (c) dissimilarity - the dissimilarity estimate, (d) (optional) target
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' r = terra::rast(system.file("ex/elev.tif", package = "terra"))
#' pr = patternogram(r)
#' pr
#' plot(pr)
patternogram = function(x, cutoff, width = cutoff/15, dist_fun = "euclidean", sample_size = 500, cloud = FALSE, target = NULL, ...){
  if (missing(cutoff)){
    cutoff = get_cutoff(x)
  }

  if (inherits(x, "SpatRaster")){
    sample_points = create_sample_points(x = x, sample_size = sample_size)
  } else if (inherits(x, "SpatVector")){
    sample_points = sf::st_as_sf(x)
  } else if (inherits(x, "sf")){
    sample_points = x
  }

  if (!is.null(target)){
    if (is.numeric(sample_points[[target]])){
      sample_points[[target]] = cut(sample_points[[target]], ...)
    }
    sample_points = split(sample_points[setdiff(names(sample_points), target)],
                          f = sample_points[[target]])
  } else {
    sample_points = list(sample_points)
  }

  distances = lapply(sample_points, calculate_distances, dist_fun = dist_fun)
  distances = lapply(distances, function(x) x[x$dist <= cutoff, ])

  if (!cloud){
    distances = lapply(distances, summarize_distances, width = width, boundary = 0)
  }

  if (!is.null(target)){
    distances = Map(cbind, distances, target = names(distances))
  }

  distances = do.call(rbind, distances)
  return(structure(distances, class = c("patternogram", class(distances))))
}
