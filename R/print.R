#' @exportS3Method
print.patternogram = function(x, ...) {
  cat("# A patternogram (tibble):", nrow(x), "by", ncol(x), "\n")
  NextMethod("print")  # falls back to tibble printing
}
