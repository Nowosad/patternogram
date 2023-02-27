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
rcl = data.frame(from = c(-Inf, 0.2, 0.4, 0.6, 0.8),
            to = c(0.2, 0.4, 0.6, 0.8, Inf),
            label = 1:5)
cls = data.frame(id = 1:5,
                 response = c("very low", "low", "medium", "high", "very high"))
response_cat = classify(response, rcl = rcl)
levels(response_cat) = cls

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
plot(dane_cat["response"])
terra::plot(pred_cv)
terra::plot(response_cat)

# testing motif
library(motif)
com = lsp_compare(pred_cv, response_cat, type = "cove", dist_fun = "jensen-shannon",
                  window = 5, output = "sf")
plot(com["dist"])

# testing patternogram
li = learner$importance()
li2 = li[gtools::mixedsort(names(li))]

predictors_scaled = scale(predictors)
predictors_scaled2 = predictors_scaled * li2

predictors3 = patternogram(predictors_scaled2, sample_size = 1000)
plot(predictors3)

predictors4 = patternogram(c(response_cat, predictors_scaled2), sample_size = 1000,
                           target = "response")
plot(predictors4)

