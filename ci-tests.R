devtools::load_all()
r = terra::rast(system.file("ex/elev.tif", package = "terra"))
system.time({pr = patternogram(r)})
pr
plot(pr)


system.time({pr2 = patternogram2(r, n_repeats = 100)})
names(pr2)[2] = "dissimilarity"
plot(pr2)


pr2$target = "A"


pr$target = "B"

pr_all = dplyr::bind_rows(pr, pr2)
plot(pr_all)

pr2$target = NULL
system.time({pr4 = patternogram(r)})
system.time({pr5 = patternogram(r)})
system.time({pr6 = patternogram(r)})
system.time({pr7 = patternogram(r)})
system.time({pr8 = patternogram(r)})
plot(pr2) +
  ggplot2::geom_point(data = pr, ggplot2::aes(x = dist, y = dissimilarity), color = "red") +
  ggplot2::geom_point(data = pr4, ggplot2::aes(x = dist, y = dissimilarity), color = "red") +
  ggplot2::geom_point(data = pr5, ggplot2::aes(x = dist, y = dissimilarity), color = "red") +
  ggplot2::geom_point(data = pr6, ggplot2::aes(x = dist, y = dissimilarity), color = "red") +
  ggplot2::geom_point(data = pr7, ggplot2::aes(x = dist, y = dissimilarity), color = "red") +
  ggplot2::geom_point(data = pr8, ggplot2::aes(x = dist, y = dissimilarity), color = "red")


# notes
# 1. There are two possible types of CI's: a bootstrap one (based on a single sample) or based on multiple samples (repeats).
# 2. There are useful for different purposes.
# 3. Spatial autocorrelation: both can be invalid if you ignore it. (?Use block bootstrap / spatially-structured resampling or model-based methods if spatial dependence is important.)
# 4. Advanced options:
# - Nested resampling: for each monte carlo, do a bootstrap
# - Block bootstrap: Instead of sampling single pixels/points, sample spatial blocks/tiles (or supercells?)
# - Modeling approach??
# 5. Technicalties: there needs to be a better mechanism for comparing patternograms ("targets")
