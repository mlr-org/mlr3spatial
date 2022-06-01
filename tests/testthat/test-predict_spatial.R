# raster predict ---------------------------------------------------------------

test_that("predictions are written to raster", {
  # [1] [2] [2]
  # [1] [1] [1]
  # [2] [2] [1]
  # [2] [2] [2]
  c_1 = y = c(1, 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2)
  raster = terra::rast(matrix(c_1, ncol = 3, byrow = TRUE))
  terra::set.names(raster, "c_1")

  # train task
  task = as_task_regr(data.table(cbind(c_1, y)), id = "test", target = "y")

  learner = LearnerRegrFeature$new()
  learner$train(task)

  # predict task
  task_predict = as_task_regr(raster, id = "test")

  # chunk size is 3 out of 12 cells
  raster = predict_spatial(task_predict, learner, chunksize = 8 * 3 * 1e-6)
  expect_equal(terra::values(raster)[, 1], y)

  # chunk size is 4 out of 12 cells
  raster = predict_spatial(task_predict, learner, chunksize = 8 * 4 * 1e-6)
  expect_equal(terra::values(raster)[, 1], y)

  # chunk size is 7 out of 12 cells
  raster = predict_spatial(task_predict, learner, chunksize = 8 * 7 * 1e-6)
  expect_equal(terra::values(raster)[, 1], y)

  # chunk size is 12 out of 12 cells
  raster = predict_spatial(task_predict, learner, chunksize = 8 * 12 * 1e-6)
  expect_equal(terra::values(raster)[, 1], y)

  # chunk size is 13 out of 12 cells
  raster = predict_spatial(task_predict, learner, chunksize = 8 * 12 * 1e-6)
  expect_equal(terra::values(raster)[, 1], y)

  # chunk size is 25 out of 12 cells
  raster = predict_spatial(task_predict, learner, chunksize = 8 * 12 * 1e-6)
  expect_equal(terra::values(raster)[, 1], y)
})

# sequential raster predict  ---------------------------------------------------

test_that("sequential execution works", {
  # train
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  layer_size = 1)
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_classif(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_class(pred, "SpatRaster")
})

test_that("sequential execution works in chunks", {
  # train
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  layer_size = 2)
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_classif(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_class(pred, "SpatRaster")
})

# parallel raster predict ------------------------------------------------------

test_that("parallel execution works with multicore", {
  skip_on_os("windows")
  # train
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  layer_size = 2)
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$parallel_predict = TRUE
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_classif(stack, id = "test")
  with_future("multicore", workers = 2, {
    pred = predict_spatial(task_predict, learner, chunksize = 1L)
  })
  expect_class(pred, "SpatRaster")
})

test_that("parallel execution works with multisession", {
  # train
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  layer_size = 2)
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$parallel_predict = TRUE
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_classif(stack, id = "test")
  with_future("multisession", workers = 2, {
    pred = predict_spatial(task_predict, learner, chunksize = 1L)
  })
  expect_class(pred, "SpatRaster")
})

test_that("parallel execution works with callr", {
  # train
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  layer_size = 2)
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$parallel_predict = TRUE
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_classif(stack, id = "test")
  with_future(future.callr::callr, workers = 2, {
    pred = predict_spatial(task_predict, learner, chunksize = 1L)
  })
  expect_class(pred, "SpatRaster")
})

# raster output formats --------------------------------------------------------

test_that("stars output works", {
  skip_if_not_installed("stars")

  # train
  stack = create_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  layer_size = 2)
  terra::crs(stack) = "EPSG:4326"
  vector = sample_stack(stack, n = 100)
  task_train = as_task_regr(vector, id = "test_vector", target = "y")
  learner = lrn("regr.rpart")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_classif(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L, format = "stars")
  expect_class(pred, "stars")
})

test_that("raster output works", {
  skip_if_not_installed("raster")
  library(raster)

  # train
  stack = create_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  layer_size = 2)
  vector = sample_stack(stack, n = 100)
  task_train = as_task_regr(vector, id = "test_vector", target = "y")
  learner = lrn("regr.rpart")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_classif(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L, format = "raster")
  expect_class(pred, "RasterLayer")
})

# raster with missing values ---------------------------------------------------

test_that("prediction on classification task works with missing values", {
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  # train task
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10)
  vector = sample_stack(stack, n = 10)
  task_train = as_task_classif(vector, id = "test_vector", target = "y")
  learner = lrn("classif.ranger")
  learner$train(task_train)

  # predict task
  stack$y = NULL
  stack = mask_stack(stack)
  task_predict = as_task_classif(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_class(pred, "SpatRaster")
  expect_true(all(is.na(terra::values(pred[["y"]])[seq(10)])))
  expect_numeric(terra::values(pred[["y"]]), any.missing = TRUE, all.missing = FALSE)
})

test_that("prediction on regression task works with missing values", {
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  # train task
  stack = create_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  vector = sample_stack(stack, n = 10)
  task_train = as_task_regr(vector, id = "test_vector", target = "y")
  learner = lrn("regr.ranger")
  learner$train(task_train)

  # predict task
  stack$y = NULL
  stack = mask_stack(stack)
  task_predict = as_task_regr(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_true(all(is.na(terra::values(pred[["y"]])[seq(10)])))
  expect_numeric(terra::values(pred[["y"]]), any.missing = TRUE, all.missing = FALSE)
})

# sequential vector predict  ---------------------------------------------------

test_that("sequential execution works", {
  # train task
  stack = create_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  vector = sample_stack(stack, n = 10)
  task_train = as_task_regr(vector, id = "test_vector", target = "y")
  learner = lrn("regr.ranger")
  learner$train(task_train)

  # predict task
  vector$y = NULL
  task_predict = as_task_regr(vector, id = "test")

  pred = predict_spatial(task_predict, learner)

  expect_class(pred, "sf")
  expect_names(names(pred), identical.to = c("y", "geometry"))
})

# parallel vector predict ------------------------------------------------------

test_that("parallel execution works with multicore", {
  skip_on_os("windows")
  # train task
  stack = create_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  vector = sample_stack(stack, n = 10)
  task_train = as_task_regr(vector, id = "test_vector", target = "y")
  learner = lrn("regr.ranger")
  learner$parallel_predict = TRUE
  learner$train(task_train)

  # predict task
  vector$y = NULL
  task_predict = as_task_regr(vector, id = "test")

  with_future("multicore", workers = 2, {
    pred = predict_spatial(task_predict, learner)
  })

  expect_class(pred, "sf")
  expect_names(names(pred), identical.to = c("y", "geometry"))
})

test_that("parallel execution works with multisession", {
  # train task
  stack = create_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  vector = sample_stack(stack, n = 10)
  task_train = as_task_regr(vector, id = "test_vector", target = "y")
  learner = lrn("regr.ranger")
  learner$parallel_predict = TRUE
  learner$train(task_train)

  # predict task
  vector$y = NULL
  task_predict = as_task_regr(vector, id = "test")

  with_future("multisession", workers = 2, {
    pred = predict_spatial(task_predict, learner)
  })

  expect_class(pred, "sf")
  expect_names(names(pred), identical.to = c("y", "geometry"))
})

test_that("parallel execution works with callr", {
  # train task
  stack = create_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  vector = sample_stack(stack, n = 10)
  task_train = as_task_regr(vector, id = "test_vector", target = "y")
  learner = lrn("regr.ranger")
  learner$parallel_predict = TRUE
  learner$train(task_train)

  # predict task
  vector$y = NULL
  task_predict = as_task_regr(vector, id = "test")

  with_future(future.callr::callr, workers = 2, {
    pred = predict_spatial(task_predict, learner)
  })

  expect_class(pred, "sf")
  expect_names(names(pred), identical.to = c("y", "geometry"))
})
