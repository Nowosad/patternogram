devtools::load_all()
library(terra)
library(future)

# create a test raster
set.seed(4100)
r = rast("/vsicurl/https://github.com/Nowosad/IIIRqueR_workshop_materials/raw/refs/heads/main/data/predictors.tif")

# 1 worker
plan(sequential)
system.time({
  pr_ui = patternogram(r, sample_size = 2000, interval = "uncertainty",
                       interval_opts = list(n_montecarlo = 10))
})
  #  user  system elapsed 
  # 2.685   0.904   6.662 

# 8 workers
plan(multisession, workers = 8)
# plan(future.mirai::mirai_multisession, workers = 8)
system.time({
  pr_ui = patternogram(r, sample_size = 2000, interval = "uncertainty",
                       interval_opts = list(n_montecarlo = 10))
})
