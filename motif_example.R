devtools::load_all()
devtools::load_all("../motif/")
library(terra)
library(sf)
library(CAST)
library(mlr3)
library(mlr3extralearners)
library(mlr3spatiotempcv)
library(ggplot2)

# read data ---------------------------------------------------------------
predictors = rast("raw-data/predictors.tif")
response = rast("raw-data/response.tif") |> setNames("response")

# create sample points ----------------------------------------------------
set.seed(2023-02-21)
dane_reg = na.omit(st_as_sf(spatSample(c(response, predictors), size = 400, as.points = TRUE)))

# regression --------------------------------------------------------------
set.seed(2023-01-15)
task = as_task_regr_st(x = dane_reg, target = "response")
learner = lrn("regr.randomForest", importance = "mse")
resampling = mlr3::rsmp("repeated_spcv_coords", folds = 5, repeats = 20) #100
rr = mlr3::resample(task = task, learner = learner, resampling = resampling)
miara1 = mlr3::mlr_measures$get("regr.mse")
miara2 = mlr3::mlr_measures$get("regr.rmse")
score_rr = rr$score(measures = c(miara1, miara2))
median(score_rr$regr.mse)
median(score_rr$regr.rmse)
learner$train(task)
pred_cv = terra::predict(predictors, model = learner$model)
plot(dane_reg["response"])
terra::plot(pred_cv, range = c(0, 1))
terra::plot(response, range = c(0, 1))

# testing patternogram
li = learner$importance()
li2 = li[gtools::mixedsort(names(li))]

predictors_scaled = scale(predictors)
predictors_scaled2 = predictors_scaled * li2

predictors3 = patternogram(predictors_scaled2, sample_size = 1000)
plot(predictors3)

predictors4 = patternogram(c(response, predictors_scaled2),
                           sample_size = 1000,
                           target = "response",
                           breaks = seq(0, 1, by = 0.2))
plot(predictors4)

