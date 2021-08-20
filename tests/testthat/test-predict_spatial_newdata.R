# SpatRaster -------------------------------------------------------------------

test_that("sequential execution works", {
  stack_classif = demo_stack_spatraster(size = 5, layers = 5)
  backend = DataBackendSpatRaster$new(stack_classif)
  task = as_task_classif(backend, target = "y", positive = "TRUE")
  # train
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  pred = predict_spatial_newdata(learner, stack_classif)
  expect_r6(pred, "Prediction")
})

test_that("parallelization works", {
  stack_classif = demo_stack_spatraster(size = 5, layers = 5)
  backend = DataBackendSpatRaster$new(stack_classif)
  task = as_task_classif(backend, target = "y", positive = "TRUE")
  # train
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))

  # parallel
  learner$parallel_predict = TRUE
  future::plan("multisession", workers = 2)
  pred = predict_spatial_newdata(learner, stack_classif)
  future::plan("sequential")
  expect_r6(pred, "Prediction")
})

test_that("supplying a filename works", {
  stack_classif = demo_stack_spatraster(size = 5, layers = 5)
  backend = DataBackendSpatRaster$new(stack_classif)
  task = as_task_classif(backend, target = "y", positive = "TRUE")
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  pred = predict_spatial_newdata(learner, stack_classif, filename = "foo.tif")
  expect_file("foo.tif")

  expect_error(predict_spatial_newdata(learner, stack_classif, filename = "foo.tif"))

  unlink(c("foo.tif", "foo.tif.aux.xml"))
})

# RasterBrick -------------------------------------------------------------------

test_that("sequential execution works", {
  stack_classif = demo_stack_rasterbrick(size = 5, layers = 5)
  backend = DataBackendRasterBrick$new(stack_classif, response = "y", response_is_factor = TRUE)
  task = as_task_classif(backend, target = "y", positive = "1")
  # train
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  pred = predict_spatial_newdata(learner, stack_classif)
  expect_r6(pred, "Prediction")
})

test_that("parallelization works", {
  stack_classif = demo_stack_rasterbrick(size = 5, layers = 5)
  backend = DataBackendRasterBrick$new(stack_classif, response = "y", response_is_factor = TRUE)
  task = as_task_classif(backend, target = "y", positive = "1")
  # train
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))

  # parallel
  learner$parallel_predict = TRUE
  future::plan("multisession", workers = 2)
  pred = predict_spatial_newdata(learner, stack_classif)
  future::plan("sequential")
  expect_r6(pred, "Prediction")
})

test_that("supplying a filename works", {
  stack_classif = demo_stack_rasterbrick(size = 5, layers = 5)
  backend = DataBackendRasterBrick$new(stack_classif, response = "y", response_is_factor = TRUE)
  task = as_task_classif(backend, target = "y", positive = "1")
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  # warning: In .gd_SetProject(object, ...) : NOT UPDATED FOR PROJ >= 6
  pred = suppressWarnings(predict_spatial_newdata(learner, stack_classif, filename = "foo.tif"))
  expect_file("foo.tif")

  expect_error(predict_spatial_newdata(learner, stack_classif, filename = "foo.tif"))

  unlink(c("foo.tif", "foo.tif.aux.xml"))
})

# sf ---------------------------------------------------------------------------

test_that("sequential execution works", {
  task = as_task_classif(backend_sf, target = "y", positive = "1")
  # train
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  pred = predict_spatial_newdata(learner, sf_pred)
  expect_r6(pred, "Prediction")
})

test_that("parallelization works", {
  task = as_task_classif(backend_sf, target = "y", positive = "1")
  # train
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))

  # parallel
  learner$parallel_predict = TRUE
  future::plan("multisession", workers = 2)
  pred = predict_spatial_newdata(learner, sf_pred)
  future::plan("sequential")
  expect_r6(pred, "Prediction")
})

test_that("supplying a filename works", {
  task = as_task_classif(backend_sf, target = "y", positive = "1")
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  pred = predict_spatial_newdata(learner, sf_pred, filename = "foo.gpkg", quiet = TRUE)
  expect_file("foo.gpkg")

  expect_error(predict_spatial_newdata(learner, sf_pred, filename = "foo.gpkg"))

  unlink(c("foo.gpkg"))
})

# stars ------------------------------------------------------------------------

test_that("sequential execution works", {
  task = as_task_regr(backend_stars, target = "X1")
  # train
  learner = lrn("regr.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  pred = predict_spatial_newdata(learner, l7data, quiet = TRUE)
  expect_r6(pred, "Prediction")
})

test_that("parallelization works", {
  task = as_task_regr(backend_stars, target = "X1")
  # train
  learner = lrn("regr.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))

  # parallel
  learner$parallel_predict = TRUE
  future::plan("multisession", workers = 2)
  pred = predict_spatial_newdata(learner, l7data, quiet = TRUE)
  future::plan("sequential")
  expect_r6(pred, "Prediction")
})

test_that("supplying a filename works", {
  task = as_task_regr(backend_stars, target = "X1")
  learner = lrn("regr.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  pred = predict_spatial_newdata(learner, l7data, filename = "foo.tif", quiet = TRUE)
  expect_file("foo.tif")

  expect_error(predict_spatial_newdata(learner, l7data, filename = "foo.tif"))

  unlink(c("foo.tif"))
})
