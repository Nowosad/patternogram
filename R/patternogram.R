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
  } else{
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

get_cutoff = function(x){
  cutoff = sqrt(terra::expanse(terra::as.polygons(terra::ext(x), crs = terra::crs(x))))
  return(cutoff)
}

summarize_distances = function(x, width, center = NULL, boundary = NULL){
  y = x |>
    dplyr::mutate(dist = ggplot2::cut_width(.data$dist, width = width,
                                            center = center, boundary = boundary)) |>
    dplyr::group_by(.data$dist) |>
    dplyr::summarise(dissimilarity = mean(.data$dissimilarity), np = dplyr::n()) |>
    dplyr::mutate(dist = get_mean_brakes(.data$dist)) |>
    dplyr::select("np", "dist", "dissimilarity")
  return(y)
}

get_mean_brakes = function(x){
  br = strsplit(as.character(x), split = "\\,")
  br = lapply(br, gsub, pattern = "[\\[\\(\\)\\]]", replacement = "", perl = TRUE)
  br = lapply(br, as.numeric)
  br = vapply(br, mean, FUN.VALUE = 1.0)
  return(br)
}

calculate_distances = function(x, dist_fun, ...){
  # value dist
  x_df = sf::st_drop_geometry(x)
  x_vdist = philentropy::distance(x_df, method = dist_fun, mute.message = TRUE, ...)
  rownames(x_vdist) = gsub("v", "", rownames(x_vdist))
  colnames(x_vdist) = gsub("v", "", colnames(x_vdist))
  x_vdist = stats::as.dist(x_vdist)

  # dist
  x_dist = sf::st_distance(x)
  x_dist = stats::as.dist(x_dist)

  # tidy
  x_vdist = broom::tidy(x_vdist)
  x_dist = broom::tidy(x_dist)

  x_dist$distance = x_dist$distance
  names(x_dist)[3] = "dist"

  # join
  all_dists = dplyr::left_join(x_dist, x_vdist, by = c("item1", "item2"))
  names(all_dists) = c("left", "right", "dist", "dissimilarity")
  return(all_dists)
}

create_sample_points_terra = function(x, sample_size){
  if (sample_size <= 1){
    sample_size = ceiling(terra::ncell(x) * sample_size)
  }
  raster_pattern = terra::spatSample(x, size = sample_size, na.rm = TRUE, as.points = TRUE)
  raster_pattern = sf::st_as_sf(raster_pattern)
  return(raster_pattern)
}

# create_sample_points_motif = function(x, sample_size, ...){
#   # type ="cove",window = 100,threshold = 0
#   raster_pattern = motif::lsp_signature(x, ...)
#   raster_pattern = motif::lsp_restructure(raster_pattern)
#   raster_pattern = motif::lsp_add_sf(raster_pattern)
#   suppressWarnings({raster_pattern = sf::st_centroid(raster_pattern)[-c(1, 2)]})
#   if (!missing(sample_size) && sample_size < nrow(raster_pattern)){
#     raster_pattern = raster_pattern[sample(seq_len(nrow(raster_pattern)), size = sample_size), ]
#   }
#   return(raster_pattern)
# }

create_sample_points = function(x, sample_size, ...){
  # if (!missing(approach) && approach == "motif"){
    # raster_pattern = create_sample_points_motif(x = x, sample_size = sample_size, ...)
  # } else {
    raster_pattern = create_sample_points_terra(x = x, sample_size = sample_size)
  # }
  return(raster_pattern)
}
