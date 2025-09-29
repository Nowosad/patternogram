#' @exportS3Method
print.patternogram = function(x, ...) {
  cat("# A patternogram (tibble):", nrow(x), "×", ncol(x), "\n")
  NextMethod("print")  # falls back to tibble printing
}