devtools::load_all()
library(terra)
library(ggplot2)
ta = rast(system.file("raster/ta_scaled.tif", package = "spquery"))
plot(ta)
my_dists = patternogram(ta, cloud = TRUE)

ggplot(my_dists, aes(dist_km, distance)) +
  geom_point()

my_dists2 = patternogram(ta)

ggplot(my_dists2, aes(dist_km, distance)) +
  geom_point()

# sc ----------------------------------------------------------------------
sc_ta = supercells::supercells(ta, compactness = 1, step = 80, clean = FALSE)

plot(ta[[1]])
plot(vect(sc_ta), add = TRUE, border = "red")
