# create a test raster
set.seed(4100)
r = terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
terra::values(r) = rnorm(terra::ncell(r))

# create a test point vector
sf::sf_use_s2(FALSE)
pts = sf::st_sf(geom = sf::st_sample(sf::st_as_sfc(sf::st_bbox(r)), size = 100))
pts$values = rnorm(nrow(pts))

# calculate various patternograms
pr = patternogram(r, sample_size = 100)
pr2 = patternogram(r, sample_size = 1)
pr_ci = patternogram(r, sample_size = 80, interval = "confidence")
pr_ui = patternogram(r, sample_size = 80, interval = "uncertainty",
                     interval_opts = list(n_montecarlo = 10))

pp = patternogram(pts, sample_size = 100)
pc = patternogram(pts, cloud = TRUE)
pts2 = pts
pts2$cat = cut(pts$values, breaks = c(-Inf, 0, Inf))
pg = patternogram(pts2, group = "cat")
pg_ui = patternogram(pts2, sample_size = 0.8, group = "cat",
                     interval = "uncertainty",
                     interval_opts = list(n_montecarlo = 10))

# test patternogram with raster input
test_that("patternogram with raster input returns a tibble with expected columns", {
  expect_true("patternogram" %in% class(pr))
  expect_true(all(c("np", "dist", "dissimilarity") %in% names(pr)))
  expect_true(nrow(pr) > 0)
  expect_true(all(c("np", "dist", "dissimilarity", "ci_lower", "ci_upper") %in% names(pr_ci)))
  expect_true(all(c("np", "dist", "dissimilarity", "ui_lower", "ui_upper") %in% names(pr_ui)))
})

# test patternogram with point vector input
test_that("patternogram with point vector input returns a tibble with expected columns", {
  expect_true("patternogram" %in% class(pp))
  expect_true(all(c("np", "dist", "dissimilarity") %in% names(pp)))
  expect_true(nrow(pp) > 0)
})

test_that("the sample size argument works as expected", {
  expect_identical(pr, pr2)
})
# test patternogram with cloud option
test_that("patternogram with cloud option returns a tibble with expected columns", {
  expect_true("patternogram" %in% class(pc))
  expect_true(all(c("left", "right", "dist", "dissimilarity") %in% names(pc)))
  expect_true(nrow(pc) > 0)
})

test_that("patternogram structure is identical for SpatRaster and sf inputs", {
  expect_identical(names(pr), names(pp))
  expect_identical(nrow(pr), nrow(pp))
})

# test patternogram with group option
test_that("patternogram with target option returns a tibble with expected columns", {
  expect_true("patternogram" %in% class(pg))
  expect_true(all(c("np", "dist", "dissimilarity", "group") %in% names(pg)))
  expect_true(nrow(pg) > 0)
  expect_true(all(c("np", "dist", "dissimilarity", "ui_lower", "ui_upper", "group") %in% names(pg_ui)))
})

test_that("patternogram cloud does not work with the uncertainty interval", {
  expect_error(patternogram(r, cloud = TRUE, interval = "uncertainty"))
})

# test plot method
# test_that("plot method works for patternogram objects", {
#   expect_silent(plot(pr))
#   expect_silent(plot(pp))
#   expect_silent(plot(pc))
#   expect_silent(plot(pt))
# })
