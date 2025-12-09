# create a test raster
set.seed(4100)
r = terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
terra::values(r) = sample(c(1, 2), size = terra::ncell(r), replace = TRUE)
r_rev = terra::ifel(r == 1, 2, 1)

# new distance measure
hamming = function(a, b) {
  if (length(a) != length(b)) stop("Lengths differ")
  sum(a != b)
}

# test
pr = patternogram(r, dist_fun = hamming, sample_size = 100)
pr_rev = patternogram(r_rev, dist_fun = hamming, sample_size = 100)
test_that("patternogram with custom distance function works as expected", {
  expect_true("patternogram" %in% class(pr))
  expect_true(all(c("np", "dist", "dissimilarity") %in% names(pr)))
  expect_true(nrow(pr) > 0)
  expect_identical(pr, pr_rev)
  expect_identical(pr, pr_rev)
})
