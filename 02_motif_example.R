devtools::load_all()
library(terra)
library(motif)
library(ggplot2)
landcover = rast(system.file("raster/landcover2015.tif", package = "motif"))
plot(landcover)

landcover_cove = lsp_signature(landcover, type = "cove", threshold = 0, window = 10)
landcover_cove = lsp_add_terra(landcover_cove)[[-c(1, 2)]]

landcover_dists = patternogram(landcover_cove, dist_fun = "jensen-shannon",
                               cloud = TRUE)

ggplot(landcover_dists, aes(dist_km, distance)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA))

landcover_dists2 = patternogram(landcover_cove, dist_fun = "jensen-shannon",
                                sample_size = 1000)
ggplot(landcover_dists2, aes(dist_km, distance)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA))

landcover_dists3 = patternogram(landcover_cove, dist_fun = "jensen-shannon",
                                sample_size = 1000,
                                cutoff = 260, width = 15)

ggplot(landcover_dists3, aes(dist_km, distance)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA))

landcover_dists4 = patternogram(landcover_cove, dist_fun = "jensen-shannon",
                                sample_size = 1000,
                                cutoff = 600, width = 15)

ggplot(landcover_dists4, aes(dist_km, distance)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA))

# sc ----------------------------------------------------------------------
library(motif)
library(sf)
sc_landcover = supercells::supercells(landcover_cove, dist_fun = "jensen-shannon",
                                      compactness = 0.3, step = 18, clean = TRUE)

plot(landcover)
plot(vect(sc_landcover), add = TRUE, border = "red")

sc_landcover_df = st_drop_geometry(sc_landcover)[-c(1:3)]
sc_landcover_dfk = kmeans(sc_landcover_df, 4)
sc_landcover$k = sc_landcover_dfk$cluster
sc_landcover2 = aggregate(sc_landcover, list(sc_landcover$k), mean)
sc_landcover3 = st_cast(sc_landcover2, "POLYGON")

plot(landcover)
plot(vect(sc_landcover3), add = TRUE, border = "red")

plot(sc_landcover3["k"])
