devtools::load_all()
library(terra)
library(ggplot2)

# approach 1 --------------------------------------------------------------
dem30m = rast("raw-data/dem30m.tif")
plot(dem30m)

my_dists = patternogram(dem30m, cloud = TRUE, sample_size = 1000)

ggplot(my_dists, aes(dist_km, distance)) +
  geom_point()

my_dists2 = patternogram(dem30m, sample_size = 1000, width = 1)

ggplot(my_dists2, aes(dist_km, distance)) +
  geom_point()

# approach 2 --------------------------------------------------------------
cove = rast("raw-data/geom_s3_cove.tif")

cove_dists = patternogram(cove, dist_fun = "jensen-shannon",
                               cloud = TRUE)

ggplot(cove_dists, aes(dist_km, distance)) +
  geom_point()

cove_dists2 = patternogram(cove, dist_fun = "jensen-shannon",
                                sample_size = 1000, width = 1)
ggplot(cove_dists2, aes(dist_km, distance)) +
  geom_point()
