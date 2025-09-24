#' @export
c.patternogram = function(..., ids = NULL) {
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
