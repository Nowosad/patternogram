
# create a test raster
r = terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
terra::values(r) = rnorm(terra::ncell(r))

# create a test point vector
sf::sf_use_s2(FALSE)
pts = sf::st_sf(geom = sf::st_sample(sf::st_as_sfc(sf::st_bbox(r)), size = 100))
pts$values = rnorm(nrow(pts))

# test patternogram with raster input
test_that("patternogram with raster input returns a tibble with expected columns", {
  pr = patternogram(r)
  expect_true("patternogram" %in% class(pr))
  expect_true(all(c("np", "dist", "dissimilarity") %in% names(pr)))
  expect_true(nrow(pr) > 0)
})

# test patternogram with point vector input
test_that("patternogram with point vector input returns a tibble with expected columns", {
  pp = patternogram(pts)
  expect_true("patternogram" %in% class(pp))
  expect_true(all(c("np", "dist", "dissimilarity") %in% names(pp)))
  expect_true(nrow(pp) > 0)
})

# test patternogram with cloud option
test_that("patternogram with cloud option returns a tibble with expected columns", {
  pc = patternogram(pts, cloud = TRUE)
  expect_true("patternogram" %in% class(pc))
  expect_true(all(c("dist", "dissimilarity") %in% names(pc)))
  expect_true(nrow(pc) > 0)
})

# test patternogram with target option
test_that("patternogram with target option returns a tibble with expected columns", {
  pts2 = pts
  pts2$cat = cut(pts$values, breaks = c(-Inf, 0, Inf))
  pt = patternogram(pts2, target = "cat")
  expect_true("patternogram" %in% class(pt))
  expect_true(all(c("np", "dist", "dissimilarity", "target") %in% names(pt)))
  expect_true(nrow(pt) > 0)
})

# test plot method
# test_that("plot method works for patternogram objects", {
#   expect_silent(plot(pr))
#   expect_silent(plot(pp))
#   expect_silent(plot(pc))
#   expect_silent(plot(pt))
# })
