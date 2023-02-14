create_sample_points_terra = function(x, sample_size = 200){
  raster_pattern = terra::spatSample(x, size = sample_size, na.rm = TRUE, as.points = TRUE)
  raster_pattern = sf::st_as_sf(raster_pattern)
  return(raster_pattern)
}

create_sample_points_motif = function(x, sample_size, ...){
  raster_pattern = motif::lsp_signature(x, ...)
  raster_pattern = motif::lsp_restructure(raster_pattern)
  raster_pattern = motif::lsp_add_sf(raster_pattern)
  suppressWarnings({raster_pattern = sf::st_centroid(raster_pattern)[-c(1, 2)]})
  if (!missing(sample_size) && sample_size < nrow(raster_pattern)){
    raster_pattern = raster_pattern[sample(seq_len(nrow(raster_pattern)), size = sample_size), ]
  }
  return(raster_pattern)
}

create_sample_points = function(x, sample_size, approach, ...){
  if (!missing(approach) && approach == "motif"){
    raster_pattern = create_sample_points_motif(x = x, sample_size = sample_size, ...)
  } else {
    raster_pattern = create_sample_points_terra(x = x, sample_size = sample_size)
  }
  return(raster_pattern)
}

calculate_distances = function(x, method, ...){
  # value dist
  x_df = sf::st_drop_geometry(x)
  x_vdist = philentropy::distance(x_df, method = method, ...)
  rownames(x_vdist) = gsub("v", "", rownames(x_vdist))
  colnames(x_vdist) = gsub("v", "", colnames(x_vdist))
  x_vdist = as.dist(x_vdist)

  # dist
  x_dist = as.dist(sf::st_distance(x))

  # tidy
  x_vdist = broom::tidy(x_vdist)
  x_dist = broom::tidy(x_dist)

  x_dist$distance = x_dist$distance / 1000
  names(x_dist)[3] = "dist_km"

  # join
  all_dists = dplyr::left_join(x_dist, x_vdist, by = c("item1", "item2"))
  return(all_dists)
}

summarize_distances = function(x, n){
  x |>
    dplyr::mutate(dist_groups = ggplot2::cut_interval(dist_km, n = n)) |>
    dplyr::group_by(dist_groups) |>
    dplyr::summarise(distance = mean(distance), n = dplyr::n())
}
