# calculate cutoff as a square root of the raster area
get_cutoff = function(x){
  cutoff = sqrt(terra::expanse(terra::as.polygons(terra::ext(x), crs = terra::crs(x))))
  return(cutoff)
}

make_breaks = function(cutoff, breaks) {
  if (length(breaks) == 1) {
    width = cutoff / breaks
    breaks = seq(0, cutoff, by = width)
  }
  return(breaks)
}

get_mean_brakes = function(x){
  br = strsplit(as.character(x), split = "\\,")
  br = lapply(br, gsub, pattern = "[\\[\\(\\)\\]]", replacement = "", perl = TRUE)
  br = lapply(br, as.numeric)
  br = vapply(br, mean, FUN.VALUE = 1.0)
  return(br)
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


create_sample_points_terra = function(x, sample_size){
  if (inherits(x, "SpatRaster")){
    if (sample_size <= 1){
      sample_size = ceiling(terra::ncell(x) * sample_size)
    }
    selected_points = terra::spatSample(x, size = sample_size, method = "random",
                                       na.rm = TRUE, as.points = TRUE)
    selected_points = sf::st_as_sf(selected_points)
  } else if (inherits(x, "sf")){
    if (sample_size <= 1){
      sample_size = ceiling(nrow(x) * sample_size)
    } else if (sample_size > nrow(x)){
      sample_size = nrow(x)
      warning("The specified sample size is larger than number of points. Using all points.",
              call. = FALSE)
    }
    selected_points = x[sample(seq_len(nrow(x)), size = sample_size), ]
  }
  return(selected_points)
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

# summarize_distances = function(x, width, center = NULL, boundary = NULL){
#   y = x |>
#     dplyr::mutate(dist = ggplot2::cut_width(.data$dist, width = width,
#                                             center = center, boundary = boundary)) |>
#     dplyr::group_by(.data$dist) |>
#     dplyr::summarise(dissimilarity = mean(.data$dissimilarity), np = dplyr::n()) |>
#     dplyr::mutate(dist = get_mean_brakes(.data$dist)) |>
#     dplyr::select("np", "dist", "dissimilarity")
#   return(y)
# }

# summarize_distances = function(x, width, center = NULL, boundary = NULL, n_bootstrap = 100, conf_level = 0.98) {
#   y = x |>
#     dplyr::mutate(dist = ggplot2::cut_width(.data$dist, width = width, center = center, boundary = boundary)) |>
#     dplyr::group_by(.data$dist) |>
#     dplyr::summarise(
#       ci = list(calculate_ci(.data$dissimilarity, n_bootstrap, conf_level)),
#       dissimilarity = mean(.data$dissimilarity),
#       np = dplyr::n()
#     ) |>
#     dplyr::mutate(
#       dist = get_mean_brakes(.data$dist),
#       ci_lower = purrr::map_dbl(.data$ci, 1),
#       ci_upper = purrr::map_dbl(.data$ci, 2)
#     ) |>
#     dplyr::select("np", "dist", "dissimilarity", "ci_lower", "ci_upper")
#   return(y)
# }

# calculate confidence intervals using bootstrap
calculate_ci = function(dissimilarities, n_bootstrap = 100, conf_level = 0.95) {
  if (length(dissimilarities) <= 1){
    return(c(NA, NA))
  } else {
    bootstrap_samples = replicate(n_bootstrap, {
      sample_dissimilarities = sample(dissimilarities, replace = TRUE)
      mean(sample_dissimilarities)
    })
    ci_lower = stats::quantile(bootstrap_samples, probs = (1 - conf_level) / 2)
    ci_upper = stats::quantile(bootstrap_samples, probs = 1 - (1 - conf_level) / 2)
    return(c(ci_lower, ci_upper))
  }
}

summarize_distances = function(x, breaks,
                               n_bootstrap = 100, conf_level = 0.95) {
  y = x |>
    dplyr::mutate(dist = cut(.data$dist, breaks = breaks, include.lowest = TRUE))

  if (missing(n_bootstrap) || n_bootstrap <=1){
    y = y |>
      dplyr::group_by(.data$dist, .drop = FALSE) |>
      dplyr::summarise(
        dissimilarity = mean(.data$dissimilarity, na.rm = TRUE),
        np = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        dist = get_mean_brakes(.data$dist)
      ) |>
      dplyr::select("np", "dist", "dissimilarity")
  } else {
    y = y |>
      dplyr::group_by(.data$dist, .drop = FALSE) |>
      dplyr::summarise(
        ci = list(calculate_ci(.data$dissimilarity, n_bootstrap, conf_level)),
        dissimilarity = mean(.data$dissimilarity, na.rm = TRUE),
        np = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        dist = get_mean_brakes(.data$dist),
        ci_lower = vapply(.data$ci, function(x) x[1], numeric(1)),
        ci_upper = vapply(.data$ci, function(x) x[2], numeric(1))
      ) |>
      dplyr::select("np", "dist", "dissimilarity", "ci_lower", "ci_upper")
  }
  return(y)
}
