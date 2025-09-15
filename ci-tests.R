devtools::load_all()
r = terra::rast(system.file("ex/elev.tif", package = "terra"))
system.time({pr = patternogram(r)})
pr
plot(pr)

system.time({pr2 = patternogram2(r, sample_size = 100, n_repeats = 50,
                                 conf_level = 0.9)})
names(pr2)[2] = "dissimilarity"
# plot(pr2)

pr2$target = "A"
pr$target = "B"

pr_all = dplyr::bind_rows(pr, pr2)
plot(pr_all)

pr2$target = NULL

my_n = 100
system.time({
pr_many = replicate(my_n, patternogram(r), simplify = FALSE) |>
  do.call(rbind, args = _) |>
  dplyr::mutate(target = rep(1:my_n, each = 15))
})

plot(pr2) +
  ggplot2::geom_point(data = pr, ggplot2::aes(x = dist, y = dissimilarity), color = "red") +
  ggplot2::geom_point(data = pr_many, ggplot2::aes(x = dist, y = dissimilarity,
                                                   color = as.factor(target), group = as.factor(target))) +
  ggplot2::geom_line(data = pr_many, ggplot2::aes(x = dist, y = dissimilarity,
                                                 color = as.factor(target), group = as.factor(target)))


# notes
# 1. There are two possible types of CI's: a bootstrap one (based on a single sample) or based on multiple samples (repeats).
# 2. There are useful for different purposes.
# 3. Advanced options:
# - Nested resampling: for each monte carlo, do a bootstrap
# - Block bootstrap: Instead of sampling single pixels/points, sample spatial blocks/tiles (or supercells?)
# - Modeling approach?? (probably not)
# 4. Technicalties: there needs to be a better mechanism for comparing patternograms ("targets")
# 5. Technicalties 2: add an option of calculating patternograms per layers and per all data
# 6. Can one type of CI, inform something about the other one? E.g., if one is fairly stable, would we expect the same from the second one?
