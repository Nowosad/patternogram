#' Combine Patternogram Objects
#'
#' Combines multiple `patternogram` objects into a single object, binding them by rows and adding an `id` column to indicate the origin.
#'
#' @param ... One or more objects of class `patternogram`.
#' @param ids Optional character vector of identifiers to assign to each input object. If `NULL`, identifiers are generated automatically.
#'
#' @return A `patternogram` object containing all input objects combined row-wise, with an added `id` column indicating their origin
#'
#' @examples
#' r = terra::rast(system.file("ex/elev.tif", package = "terra"))
#' r1 = r[r < 340, drop = FALSE]
#' r2 = r[r >= 340, drop = FALSE]
#' pr1 = patternogram(r1)
#' pr2 = patternogram(r2)
#' pr_all = bind_patternogram(pr1, pr2, ids = c("low", "high"))
#' plot(pr_all)
#' @export
bind_patternogram = function(..., ids = NULL) {
  # patternograms = c(list(x), list(...))
  patternograms = list(...)

  if (is.null(ids)) {
    ids = paste0("patternogram_", seq_along(patternograms))
  }

  out = do.call(dplyr::bind_rows, Map(function(df, id) {
    df$id = id
    df
  }, patternograms, ids))

  out
}
