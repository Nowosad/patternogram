#' Create a Patternogram or a Patternogram Cloud
#'
#' It creates a patternogram or a patternogram cloud. The function takes a raster object of class `SpatRaster` or a point vector object of class `sf` as input, and calculates the dissimilarity between values of pairs of points given the distances between them. The output of this function is a tibble of the patternogram class that can be visualized with the `plot()` and `autoplot()` functions.
#'
#' @param x A raster object of class SpatRaster (terra) or a point vector object of class sf (sf)
#' @param cutoff Spatial distance up to which point pairs are included in patternogram estimates.
#'   By default: a square root of the raster/point data area
#' @param breaks Either the number of distance intervals (breaks) to be used in calculations, or a numeric vector specifying the break points. If a single number is provided, it specifies the number of intervals, and the function will create equally spaced intervals from 0 to `cutoff`. By default: 15
#' @param dist_fun Distance measure used. This function uses the `philentropy::distance()` function (run `philentropy::getDistMethods()` to find possible distance measures). By default: "euclidean"
#' @param sample_size Only used when `x` is raster. Proportion of the cells/points to be used in calculations. Value between 0 and 1. It is also possible to specify an integer larger than 1, in which case the specified number of cells/points will be used in calculations by random sampling. By default: 500

#' @param interval Type of interval to be calculated. Options are "none" (default), "confidence" (confidence intervals around the mean dissimilarity estimate), and "uncertainty" (uncertainty intervals around the dissimilarity estimates). The confidence intervals are calculated using a bootstrap approach, while the uncertainty intervals are calculated using a Monte Carlo approach. Note that uncertainty intervals require more computations than confidence intervals. Also, confidence intervals are only available when `cloud = FALSE`.
#' @param interval_opts A list of options for interval calculations. Possible options are (a) `conf_level`: confidence level for intervals (default: 0.95), (b) `n_bootstrap`: number of bootstrap samples for confidence intervals (default: 100), and (c) `n_montecarlo`: number of Monte Carlo repetitions for uncertainty intervals (default: 100)
#' @param group Optional; name of a column in the point attribute table to calculate separate patternograms for different categories or ranges of a numeric variable. If the specified column is numeric, it will be converted to a factor using the `base::cut()` function. By default: NULL
#' @param cloud Logical; if TRUE, return the patternogram cloud
#' @param ... Not used
#'
#' @return A tibble of the patternogram class with columns (a) np: the number of point pairs in this estimate, (b) dist: the middle of the distance interval used for each estimate, (c) dissimilarity: the dissimilarity estimate.
#' Additionally, if `interval = "confidence"`, the tibble contains columns (d) ci_lower: lower confidence interval, and (e) ci_upper: upper confidence interval. If `interval = "uncertainty"`, the tibble contains columns (d) ui_lower: lower uncertainty interval, and (e) ui_upper: upper uncertainty interval.
#' If `group` is specified, the tibble also contains a column (f) group: the group category.
#' Also note, that if `interval = "uncertainty"`, the np is the mean number of point pairs across Monte Carlo repetitions, and the dissimilarity is the mean dissimilarity across Monte Carlo repetitions.
#'
#' If `cloud = TRUE`, the outcome is a tibble of the patternogram class with columns (a) left: ID of the first point in the pair, (b) right: ID of the second point in the pair, (c) dist: the spatial distance between the points in the pair, and (d) dissimilarity: the dissimilarity between the values of the points in the pair.
#' If `group` is specified, the tibble also contains a column (e) group: the group category.
#'
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' r = terra::rast(system.file("ex/elev.tif", package = "terra"))
#' pr = patternogram(r)
#' pr
#' plot(pr)
patternogram = function(x, cutoff, breaks = 15, dist_fun = "euclidean",
                        sample_size = 500,
                        interval = c("none", "confidence", "uncertainty"),
                        interval_opts = list(conf_level = 0.95,
                                             n_bootstrap = 100,
                                             n_montecarlo = 100),
                        group = NULL,
                        cloud = FALSE,
                         ...) {

  do_warn = ifelse(missing(sample_size), FALSE, TRUE)

  if (missing(cutoff)){
    cutoff = get_cutoff(x)
  }

  if (inherits(x, "SpatVector")){
    sample_points_base = sf::st_as_sf(x) # nocov
  } else if (inherits(x, "sf")){
    sample_points_base = x
  } else {
    sample_points_base = NULL
  }

  conf_level = ifelse(is.null(interval_opts$conf_level), 0.95, interval_opts$conf_level)
  n_bootstrap = ifelse(is.null(interval_opts$n_bootstrap), 100, interval_opts$n_bootstrap)
  n_montecarlo = ifelse(is.null(interval_opts$n_montecarlo), 100, interval_opts$n_montecarlo)
  if (!missing(interval) && interval == "confidence"){
    n_montecarlo = 1
  } else if (!missing(interval) && interval == "uncertainty"){
    n_bootstrap = 1
  } else {
    n_bootstrap = 1; n_montecarlo = 1
  }

  breaks = make_breaks(cutoff, breaks)

  if (n_montecarlo == 1){
    if (inherits(x, "SpatRaster")){
      sample_points = create_sample_points(x = x,
                                           sample_size = sample_size,
                                           warning = do_warn)
    } else {
      sample_points = create_sample_points(x = sample_points_base,
                                           sample_size = sample_size,
                                           warning = do_warn)
    }
    result = single_patternogram(sample_points, cutoff = cutoff, breaks = breaks,
                        dist_fun = dist_fun, cloud = cloud,
                        group = group, n_bootstrap = n_bootstrap,
                        conf_level = conf_level, ...)
  } else if (cloud){
    stop("Monte Carlo uncertainty estimation not implemented for patternogram clouds.",
         call. = FALSE)
  } else {
    # Monte Carlo: repeat sampling + summarization
    if (requireNamespace("future.apply", quietly = TRUE) &&
        requireNamespace("future", quietly = TRUE) &&
        !identical(future::plan("list"),
                   list(list(strategy = "sequential")))) {
      if (inherits(x, "SpatRaster")) x = terra::wrap(x)
      results = future.apply::future_lapply(seq_len(n_montecarlo), function(r) {
        if (inherits(x, "PackedSpatRaster")) {
          sample_points = create_sample_points(x = terra::rast(x),
                                               sample_size = sample_size)
        } else {
          sample_points = create_sample_points(x = sample_points_base,
                                               sample_size = sample_size)
        }

        single_patternogram(sample_points, cutoff = cutoff, breaks = breaks,
                            dist_fun = dist_fun, cloud = cloud,
                            group = group, n_bootstrap = n_bootstrap,
                            conf_level = conf_level, ...)
      },
      future.globals = c("single_patternogram", "create_sample_points"),
      future.packages = c("terra", "patternogram"),
      future.seed = TRUE)
    } else {
      results = vector("list", n_montecarlo)
      for (r in seq_len(n_montecarlo)) {
        if (inherits(x, "SpatRaster")){
          sample_points = create_sample_points(x = x, sample_size = sample_size)
        } else {
          sample_points = create_sample_points(x = sample_points_base,
                                               sample_size = sample_size)
        }
        results[[r]] = single_patternogram(sample_points, cutoff = cutoff, breaks = breaks,
                                           dist_fun = dist_fun, cloud = cloud,
                                           group = group, n_bootstrap = n_bootstrap,
                                           conf_level = conf_level, ...)
      }
    }
    # combine results: assume each run has dist + dissimilarity (and maybe group)
    combined = dplyr::bind_rows(results, .id = "repeat_id")

    # make sure all bins exist in every repeat
    all_bins = unique(combined$dist)

    if (is.null(group)){
      combined = tidyr::complete(combined, .data$repeat_id, dist = all_bins,
                                 fill = list(np = 0, dissimilarity = NA))
    } else {
      combined = tidyr::complete(combined, .data$repeat_id, dist = all_bins,
                                 group = unique(combined$group),
                                 fill = list(np = 0, dissimilarity = NA))
    }

    # summarize across repeats: mean dissimilarity & CI per bin
    alpha_low = (1 - conf_level) / 2
    alpha_high = 1 - alpha_low

    result = combined |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c("group", "dist")))) |>
      dplyr::summarise(
        mean_np = as.integer(ceiling(mean(.data$np, na.rm = TRUE))),
        mean_dissimilarity = mean(.data$dissimilarity, na.rm = TRUE),
        # ci_lower = mean(ci_lower, na.rm = TRUE),
        # ci_upper = mean(ci_upper, na.rm = TRUE),
        ui_lower = stats::quantile(.data$dissimilarity, probs = alpha_low, na.rm = TRUE),
        ui_upper = stats::quantile(.data$dissimilarity, probs = alpha_high, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::select(np = "mean_np", "dist", dissimilarity = "mean_dissimilarity",
                    "ui_lower", "ui_upper", dplyr::any_of("group"))
  }
  rownames(result) = NULL
  return(structure(result, class = c("patternogram", class(result))))
}
