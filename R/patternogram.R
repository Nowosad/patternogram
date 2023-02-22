#' Title
#'
#' @param x
#' @param cutoff
#' @param width
#' @param dist_fun
#' @param sample_size
#' @param cloud
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
patternogram = function(x, cutoff, width = cutoff/15, dist_fun = "euclidean", sample_size = 100, cloud = FALSE, target = NULL, ...){
  if (missing(cutoff)){
    cutoff = get_cutoff(x)
  }
  sample_points = create_sample_points(x = x, sample_size = sample_size)
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
  distances = lapply(distances, function(x) x[x$dist_km <= cutoff, ])
  if (!cloud){
    distances = lapply(distances, summarize_distances, width = width, boundary = 0)
  }
  if (!is.null(target)){
    distances = Map(cbind, distances, target = names(distances))
  }
  distances = do.call(rbind, distances)
  return(distances)
}

get_cutoff = function(x){
  cutoff = sqrt(terra::expanse(terra::as.polygons(terra::ext(x), crs = terra::crs(x))) / 1000000)
  return(cutoff)
}

summarize_distances = function(x, width, center = NULL, boundary = NULL){
  y = x |>
    dplyr::mutate(dist_km = ggplot2::cut_width(dist_km, width = width,
                                                   center = center, boundary = boundary)) |>
    dplyr::group_by(dist_km) |>
    dplyr::summarise(distance = mean(distance), n = dplyr::n()) |>
    dplyr::mutate(dist_km = get_mean_brakes(dist_km))
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
  x_vdist = philentropy::distance(x_df, method = dist_fun,
                                  mute.message = TRUE, ...)
  rownames(x_vdist) = gsub("v", "", rownames(x_vdist))
  colnames(x_vdist) = gsub("v", "", colnames(x_vdist))
  x_vdist = stats::as.dist(x_vdist)

  # dist
  x_dist = stats::as.dist(sf::st_distance(x))

  # tidy
  x_vdist = broom::tidy(x_vdist)
  x_dist = broom::tidy(x_dist)

  x_dist$distance = x_dist$distance / 1000
  names(x_dist)[3] = "dist_km"

  # join
  all_dists = dplyr::left_join(x_dist, x_vdist, by = c("item1", "item2"))
  return(all_dists)
}

create_sample_points_terra = function(x, sample_size = 200){
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
