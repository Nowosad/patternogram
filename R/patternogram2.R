patternogram2 = function(x, cutoff, width = cutoff/15, dist_fun = "euclidean",
                         sample_size = 500, n_repeats = 100, cloud = FALSE,
                         target = NULL, conf_level = 0.95, ...) {
  if (missing(cutoff)){
    cutoff = get_cutoff(x)
  }

  # handle sf/vector input as before
  if (inherits(x, "SpatVector")){
    sample_points_base = sf::st_as_sf(x)
  } else if (inherits(x, "sf")){
    sample_points_base = x
  } else {
    sample_points_base = NULL
  }

  breaks = make_breaks(cutoff, width, boundary = 0)

  # helper: one run of your pipeline
  run_once = function(sample_points) {
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
      distances = lapply(distances, summarize_distances2, width = width,
                         boundary = 0, breaks = breaks)
    }

    if (!is.null(target)){
      distances = Map(cbind, distances, target = names(distances))
    }

    distances = do.call(rbind, distances)
    return(distances)
  }

  # Monte Carlo: repeat sampling + summarisation
  results = vector("list", n_repeats)
  for (r in seq_len(n_repeats)) {
    if (inherits(x, "SpatRaster")){
      sample_points = create_sample_points(x = x, sample_size = sample_size)
    } else {
      sample_points = sample_points_base
    }
    results[[r]] = run_once(sample_points)
  }

  # combine results: assume each run has dist + dissimilarity (and maybe target)
  combined = dplyr::bind_rows(results, .id = "repeat_id")

  # make sure all bins exist in every repeat
  all_bins = unique(combined$dist)

  combined = combined |>
    tidyr::complete(repeat_id, dist = all_bins,
                    fill = list(dissimilarity = NA))

  # summarise across repeats: mean dissimilarity & CI per bin
  alpha_low = (1 - conf_level) / 2
  alpha_high = 1 - alpha_low

  summarized = combined |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c("dist", "target")))) |>
    dplyr::summarise(
      mean_dissimilarity = mean(dissimilarity, na.rm = TRUE),
      ci_lower = quantile(dissimilarity, probs = alpha_low, na.rm = TRUE),
      ci_upper = quantile(dissimilarity, probs = alpha_high, na.rm = TRUE),
      .groups = "drop"
    )

  return(structure(summarized, class = c("patternogram", class(summarized))))
}

make_breaks = function(cutoff, width, center = NULL, boundary = 0) {
  # generate cut breaks manually
  seq(0, cutoff, by = width)
}

summarize_distances2 = function(x, width, center = NULL, boundary = NULL,
                                n_bootstrap = 100, conf_level = 0.98,
                                breaks = NULL) {
  if (!is.null(breaks)) {
    y = x |>
      dplyr::mutate(dist = cut(.data$dist, breaks = breaks, include.lowest = TRUE))
  } else {
    y = x |>
      dplyr::mutate(dist = ggplot2::cut_width(.data$dist, width = width,
                                              center = center, boundary = boundary))
  }

  y = y |>
    dplyr::group_by(.data$dist) |>
    dplyr::summarise(
      ci = list(calculate_ci(.data$dissimilarity, n_bootstrap, conf_level)),
      dissimilarity = mean(.data$dissimilarity, na.rm = TRUE),
      np = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dist = get_mean_brakes(.data$dist),
      ci_lower = purrr::map_dbl(.data$ci, ~ .x[1]),
      ci_upper = purrr::map_dbl(.data$ci, ~ .x[2])
    ) |>
    dplyr::select("np", "dist", "dissimilarity", "ci_lower", "ci_upper")

  return(y)
}
