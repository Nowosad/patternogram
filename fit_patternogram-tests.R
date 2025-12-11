devtools::load_all()
library(gstat)
library(ggplot2)

r = terra::rast(system.file("ex/elev.tif", package = "terra"))
pr = patternogram(r)
pr
autoplot(pr) + ggtitle("Patternogram of elevation data")

m = fit_patternogram(pr, model = vgm(model = "Sph", nugget = 30))

library(sacmetrics)
sacmetrics::vgm_ssvr(m)
sacmetrics::vgm_auc(m, maxdist = 70000)

vl = variogramLine(m, maxdist = pr$dist[15])
names(vl)[[2]] = "dissimilarity"

autoplot(pr) +
  geom_line(data = vl, color = "red") +
  ggtitle("Patternogram of elevation data")


