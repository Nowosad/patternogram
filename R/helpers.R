# calculate cutoff as a square root of the raster area
get_cutoff = function(x){
  cutoff = sqrt(terra::expanse(terra::as.polygons(terra::ext(x), crs = terra::crs(x))))
  return(cutoff)
}

make_breaks = function(cutoff, width) {
  # generate cut breaks manually
  seq(0, cutoff, by = width)
}
