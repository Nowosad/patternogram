#' @exportS3Method pillar::tbl_sum
tbl_sum.patternogram = function(x, ...) {
  # this replaces "# A tibble: 15 × 3"
  c("A patternogram" = paste0(nrow(x), " \u00d7 ", ncol(x)))
}
