patternogram3 = function(x, cutoff, width = cutoff/15, dist_fun = "euclidean",
                         sample_size = 500, cloud = FALSE,
                         group = NULL,
                         interval = c("none", "confidence", "uncertainty"),
                         interval_opts = list(conf_level = 0.95,
                                              n_bootstrap = 100,
                                              n_montecarlo = 100),
                         ...) {
  if (missing(cutoff)){
    cutoff = get_cutoff(x)
  }

  if (inherits(x, "SpatVector")){
    sample_points_base = sf::st_as_sf(x)
  } else if (inherits(x, "sf")){
    sample_points_base = x
  } else {
    sample_points_base = NULL
  }

  conf_level = interval_opts$conf_level
  n_bootstrap = interval_opts$n_bootstrap
  n_montecarlo = interval_opts$n_montecarlo
  if (!missing(interval) && interval == "confidence"){
    n_montecarlo = 1
  } else if (!missing(interval) && interval == "uncertainty"){
    n_bootstrap = 1
  } else {
    n_bootstrap = 1; n_montecarlo = 1
  }

  breaks = make_breaks(cutoff, width)

  if (n_montecarlo == 1){
    if (inherits(x, "SpatRaster")){
      sample_points = create_sample_points(x = x, sample_size = sample_size)
    } else {
      sample_points = create_sample_points(x = sample_points_base, sample_size = sample_size)
    }
    result = single_patternogram(sample_points, cutoff = cutoff, width = width,
                        dist_fun = dist_fun, cloud = cloud,
                        group = group, breaks = breaks, n_bootstrap = n_bootstrap, ...)
  } else {
    # Monte Carlo: repeat sampling + summarization
    results = vector("list", n_montecarlo)
    for (r in seq_len(n_montecarlo)) {
      if (inherits(x, "SpatRaster")){
        sample_points = create_sample_points(x = x, sample_size = sample_size)
      } else {
        sample_points = create_sample_points(x = sample_points_base, sample_size = sample_size)
      }
      results[[r]] = single_patternogram(sample_points, cutoff = cutoff, width = width,
                                         dist_fun = dist_fun, cloud = cloud,
                                         group = group, breaks = breaks, n_bootstrap = n_bootstrap, ...)
    }

    # combine results: assume each run has dist + dissimilarity (and maybe group)
    combined = dplyr::bind_rows(results, .id = "repeat_id")

    # make sure all bins exist in every repeat
    all_bins = unique(combined$dist)

    combined = combined |>
      tidyr::complete(repeat_id, dist = all_bins,
                      fill = list(dissimilarity = NA))

    # summarise across repeats: mean dissimilarity & CI per bin
    alpha_low = (1 - conf_level) / 2
    alpha_high = 1 - alpha_low

    result = combined |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c("dist", "group")))) |>
      dplyr::summarise(
        mean_dissimilarity = mean(dissimilarity, na.rm = TRUE),
        # ci_lower = mean(ci_lower, na.rm = TRUE),
        # ci_upper = mean(ci_upper, na.rm = TRUE),
        ui_lower = quantile(dissimilarity, probs = alpha_low, na.rm = TRUE),
        ui_upper = quantile(dissimilarity, probs = alpha_high, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::rename(dissimilarity = mean_dissimilarity)
  }
  return(structure(result, class = c("patternogram", class(result))))
}

single_patternogram = function(sample_points, cutoff, width = cutoff/15,
                               dist_fun = "euclidean", cloud = FALSE,
                               group = NULL, breaks = NULL,
                               n_bootstrap, ...) {

  if (!is.null(group)){
    if (is.numeric(sample_points[[group]])){
      sample_points[[group]] = cut(sample_points[[group]], ...)
    }
    sample_points = split(sample_points[setdiff(names(sample_points), group)],
                          f = sample_points[[group]])
  } else {
    sample_points = list(sample_points)
  }

  distances = lapply(sample_points, calculate_distances, dist_fun = dist_fun)
  distances = lapply(distances, function(x) x[x$dist <= cutoff, ])

  if (!cloud){
    distances = lapply(distances, summarize_distances, width = width,
                       n_bootstrap = n_bootstrap, conf_level = conf_level,
                       boundary = 0, breaks = breaks)
  }

  if (!is.null(group)){
    distances = Map(cbind, distances, group = names(distances))
  }

  distances = do.call(rbind, distances)
  return(distances)
}
