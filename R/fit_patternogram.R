#' Title
#'
#' @param x
#' @param model
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
fit_patternogram = function(x, model, ...) {
  vg = as.gstatVariogram.patternogram(x)
  fit = gstat::fit.variogram(vg, model, ...)
  return(fit)
}

as.gstatVariogram.patternogram = function(x) {
  vg = as.data.frame(x)

  # Standardize column names
  names(vg)[names(vg) == "dissimilarity"] = "gamma"

  # Ensure correct types
  vg$np    = as.numeric(vg$np)

  # Add other required fields with default values
  vg$dir.hor = 0
  vg$dir.ver = 0
  vg$id = "var1"

  # Assign gstat variogram class
  class(vg) = c("gstatVariogram", "data.frame")

  return(vg)
}
