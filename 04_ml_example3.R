devtools::load_all()
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
response_cat = response > 0.65 |> setNames("response")

# create sample points ----------------------------------------------------
set.seed(2023-02-21)
# dane_reg = na.omit(st_as_sf(spatSample(c(response, predictors), size = 400, as.points = TRUE)))
dane_cat = na.omit(st_as_sf(spatSample(c(response_cat, predictors), size = 400, as.points = TRUE)))
dane_cat$response = as.factor(dane_cat$response)
# regression --------------------------------------------------------------
set.seed(2023-01-15)
task = as_task_classif_st(x = dane_cat, target = "response")
learner = lrn("classif.randomForest", importance = "gini")
resampling = mlr3::rsmp("repeated_spcv_coords", folds = 5, repeats = 20) #100
rr = mlr3::resample(task = task, learner = learner, resampling = resampling)
miara1 = mlr3::mlr_measures$get("classif.acc")
miara2 = mlr3::mlr_measures$get("classif.auc")
score_rr = rr$score(measures = c(miara1, miara2))
median(score_rr$classif.acc)
learner$train(task)
pred_cv = terra::predict(predictors, model = learner$model)
plot(dane_reg["response"])
plot(pred_cv)
plot(response_cat)

# (weighted)
li = learner$importance()
li2 = li[gtools::mixedsort(names(li))]

predictors_scaled = scale(predictors)
predictors_scaled2 = predictors_scaled * li2

predictors3 = patternogram(predictors_scaled2, sample_size = 1000)

ggplot(predictors3, aes(dist_km, distance)) +
  geom_point()

predictors4 = patternogram(c(response_cat, predictors_scaled2), sample_size = 1000,
                           target = "response")

ggplot(predictors4, aes(dist_km, distance, color = target)) +
  geom_point()
#
# predictors5a = patternogram(response_cat[response_cat, drop = FALSE], sample_size = 1000)
# predictors5a$target = TRUE
# predictors5b = patternogram(response_cat[!response_cat, drop = FALSE], sample_size = 1000)
# predictors5b$target = FALSE
#
# predictors5 = rbind(predictors5a, predictors5b)
#
# ggplot(predictors5, aes(dist_km, distance)) +
#   geom_point()
