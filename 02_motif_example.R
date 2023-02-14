devtools::load_all()
library(terra)
library(ggplot2)
landcover = rast(system.file("raster/landcover2015.tif", package = "motif"))
plot(landcover)
landcover_dists = patternogram(landcover, dist_fun = "jensen-shannon",
                               cloud = TRUE, approach = "motif",
                               type = "cove", threshold = 0, window = 10)

ggplot(landcover_dists, aes(dist_km, distance)) +
  geom_point()

landcover_dists2 = patternogram(landcover, dist_fun = "jensen-shannon",
                                approach = "motif",
                                type = "cove", threshold = 0, window = 10)

ggplot(landcover_dists2, aes(dist_km, distance)) +
  geom_point()


landcover_dists2 = patternogram(landcover, dist_fun = "jensen-shannon",
                                approach = "motif",
                                type = "cove", threshold = 0, window = 10,
                                cutoff = 260, width = 10)

ggplot(landcover_dists2, aes(dist_km, distance)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA))

# sc ----------------------------------------------------------------------
library(motif)
landcover2 = lsp_signature(landcover, type = "cove", threshold = 0, window = 10)
landcover2 = lsp_add_terra(landcover2)

sc_landcover = supercells::supercells(landcover2[[-c(1, 2)]], dist_fun = "jensen-shannon",
                                      compactness = 0.3, step = 26, clean = TRUE)

plot(landcover)
plot(vect(sc_landcover), add = TRUE, border = "red")
