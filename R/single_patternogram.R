single_patternogram = function(sample_points, cutoff, breaks,
                               dist_fun = "euclidean", cloud = FALSE,
                               group = NULL,
                               n_bootstrap, conf_level, ...) {

  if (!is.null(group) && is.character(group)){
    # if (is.numeric(sample_points[[group]])){
    #   sample_points[[group]] = cut(sample_points[[group]], ...)
    # }
    sample_points = split(sample_points[setdiff(names(sample_points), group)],
                          f = sample_points[[group]])
  } else if (isTRUE(group)) {
    attr_cols = setdiff(names(sample_points), attr(sample_points, "sf_column"))
    sample_points = lapply(attr_cols, function(col) {
      sample_points[c(col, attr(sample_points, "sf_column"))]
    })
    names(sample_points) = attr_cols
  } else {
    sample_points = list(sample_points)
  }

  distances = lapply(sample_points, calculate_distances, dist_fun = dist_fun)
  distances = lapply(distances, function(x) x[x$dist <= cutoff, ])

  if (!cloud){
    distances = lapply(distances, summarize_distances, breaks = breaks,
                       n_bootstrap = n_bootstrap, conf_level = conf_level)
  }

  if (!is.null(group)){
    distances = Map(dplyr::bind_cols, distances, group = names(distances))
  }

  distances = do.call(rbind, distances)
  return(distances)
}
