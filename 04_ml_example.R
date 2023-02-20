library(terra)
library(sf)
library(CAST)
library(mlr3)
library(mlr3extralearners)
library(mlr3spatiotempcv)

predictors = rast("raw-data/predictors.tif")
response = rast("raw-data/response.tif")
samplepoints = read_sf("raw-data/samplepoints.gpkg")
trainDat = read.csv("raw-data/trainDat.csv")

dane = cbind(samplepoints, trainDat) |>
  dplyr::select(-clstrID, -ID)

task = as_task_regr_st(x = dane, target = "response")

learner = lrn("regr.randomForest", importance = "mse")

resampling = mlr3::rsmp("repeated_spcv_coords", folds = 5, repeats = 20) #100
resampling

set.seed(2023-01-15)
rr = mlr3::resample(task = task, learner = learner, resampling = resampling)

miara1 = mlr3::mlr_measures$get("regr.rmse")
miara2 = mlr3::mlr_measures$get("regr.rsq")
score_rr = rr$score(measures = c(miara1, miara2))
score_rr
median(score_rr$regr.rmse)
median(score_rr$regr.rsq, na.rm = TRUE)

learner$train(task)

pred_cv = terra::predict(predictors, model = learner$model)
plot(dane["response"])
plot(pred_cv)

#1 (raw)
plot(predictors)
predictors1 = patternogram(predictors, sample_size = 1000)

ggplot(predictors1, aes(dist_km, distance)) +
  geom_point()

#2 (scale)
predictors_scaled = scale(predictors)
plot(predictors_scaled)

predictors2 = patternogram(predictors_scaled, sample_size = 1000)

ggplot(predictors2, aes(dist_km, distance)) +
  geom_point()

#3 (weighted)
li = learner$importance()
li2 = li[gtools::mixedsort(names(li))]

predictors_scaled2 = predictors_scaled * li2
predictors3 = patternogram(predictors_scaled2, sample_size = 1000)

ggplot(predictors3, aes(dist_km, distance)) +
  geom_point()
