# raster predictions -----------------------------------------------------------

test_that("predictions are written to raster", {
  skip_if_not_installed("paradox")
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
  task_predict = as_task_unsupervised(raster, id = "test")

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
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      factor_layer("y", levels = c("a", "b"))
    ),
    layer_size = 1
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif_st(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_unsupervised(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_class(pred, "SpatRaster")
})

test_that("sequential execution works in chunks", {
  # train
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      factor_layer("y", levels = c("a", "b"))
    ),
    layer_size = 2
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif_st(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_unsupervised(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_class(pred, "SpatRaster")
})

# probability prediction -------------------------------------------------------

test_that("probability predictions are written to raster", {
  # train
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      factor_layer("y", levels = c("a", "b"))
    ),
    layer_size = 1
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif_st(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart", predict_type = "prob")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_unsupervised(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_class(pred, "SpatRaster")
  expect_equal(terra::nlyr(pred), 2L)
  expect_named(pred, c("a", "b"))
  values = terra::values(pred)
  expect_true(all(values >= 0 & values <= 1))
  expect_equal(unname(rowSums(values)), rep(1, nrow(values)))
})

test_that("multiclass probability predictions are written to raster", {
  # train
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      factor_layer("y", levels = c("a", "b", "c"))
    ),
    layer_size = 1
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif_st(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart", predict_type = "prob")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_unsupervised(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_class(pred, "SpatRaster")
  expect_equal(terra::nlyr(pred), 3L)
  expect_named(pred, c("a", "b", "c"))
})

test_that("probability prediction works with missing values", {
  # train
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      factor_layer("y", levels = c("a", "b"))
    ),
    dimension = 100
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif_st(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart", predict_type = "prob")
  learner$train(task_train)

  # predict
  stack$y = NULL
  stack = mask_stack(stack)
  task_predict = as_task_unsupervised(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_class(pred, "SpatRaster")
  expect_named(pred, c("a", "b"))
  expect_true(all(is.na(terra::values(pred[["a"]])[seq(10)])))
  expect_numeric(terra::values(pred[["a"]]), any.missing = TRUE, all.missing = FALSE)
})

# parallel raster predict ------------------------------------------------------

test_that("parallel execution works with multicore", {
  skip_on_os("windows")
  # train
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      factor_layer("y", levels = c("a", "b"))
    ),
    layer_size = 2
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif_st(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$parallel_predict = TRUE
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_unsupervised(stack, id = "test")
  with_future("multicore", workers = 2, {
    pred = predict_spatial(task_predict, learner, chunksize = 1L)
  })
  expect_class(pred, "SpatRaster")
})

test_that("parallel execution works with multisession", {
  # train
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      factor_layer("y", levels = c("a", "b"))
    ),
    layer_size = 2
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif_st(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$parallel_predict = TRUE
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_unsupervised(stack, id = "test")
  with_future("multisession", workers = 2, {
    pred = predict_spatial(task_predict, learner, chunksize = 1L)
  })
  expect_class(pred, "SpatRaster")
})

test_that("parallel execution works with callr", {
  skip_if_not_installed("future.callr")
  # train
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      factor_layer("y", levels = c("a", "b"))
    ),
    layer_size = 2
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif_st(vector, id = "test_vector", target = "y")
  learner = lrn("classif.rpart")
  learner$parallel_predict = TRUE
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_unsupervised(stack, id = "test")
  with_future(future.callr::callr, workers = 2, {
    pred = predict_spatial(task_predict, learner, chunksize = 1L)
  })
  expect_class(pred, "SpatRaster")
})

# raster output formats --------------------------------------------------------

test_that("stars output works", {
  skip_if_not_installed("stars")
  skip_on_os("mac")

  # train
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      numeric_layer("y")
    ),
    layer_size = 2
  )
  terra::crs(stack) = "EPSG:4326"
  vector = sample_stack(stack, n = 100)
  task_train = as_task_regr_st(vector, id = "test_vector", target = "y")
  learner = lrn("regr.rpart")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_unsupervised(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L, format = "stars")
  expect_class(pred, "stars")
})

test_that("raster output works", {
  skip_if_not_installed("raster")
  library(raster)

  # train
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      numeric_layer("y")
    ),
    layer_size = 2
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_regr_st(vector, id = "test_vector", target = "y")
  learner = lrn("regr.rpart")
  learner$train(task_train)

  # predict
  stack$y = NULL
  task_predict = as_task_unsupervised(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L, format = "raster")
  expect_class(pred, "RasterLayer")
})

# raster with missing values ---------------------------------------------------

test_that("prediction on classification task works with missing values", {
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  # train task
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      factor_layer("y", levels = c("a", "b"))
    ),
    dimension = 100
  )
  vector = sample_stack(stack, n = 100)
  task_train = as_task_classif_st(vector, id = "test_vector", target = "y")
  learner = lrn("classif.ranger")
  learner$train(task_train)

  # predict task
  stack$y = NULL
  stack = mask_stack(stack)
  task_predict = as_task_unsupervised(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_class(pred, "SpatRaster")
  expect_true(all(is.na(terra::values(pred[["y"]])[seq(10)])))
  expect_numeric(terra::values(pred[["y"]]), any.missing = TRUE, all.missing = FALSE)
})

test_that("prediction on regression task works with missing values", {
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  # train task
  stack = generate_stack(
    list(
      numeric_layer("x_1"),
      numeric_layer("y")
    ),
    dimension = 100
  )
  vector = sample_stack(stack, n = 10)
  task_train = as_task_regr_st(vector, id = "test_vector", target = "y")
  learner = lrn("regr.ranger")
  learner$train(task_train)

  # predict task
  stack$y = NULL
  stack = mask_stack(stack)
  task_predict = as_task_unsupervised(stack, id = "test")
  pred = predict_spatial(task_predict, learner, chunksize = 1L)
  expect_true(all(is.na(terra::values(pred[["y"]])[seq(10)])))
  expect_numeric(terra::values(pred[["y"]]), any.missing = TRUE, all.missing = FALSE)
})

# vector prediction ------------------------------------------------------------

test_that("prediction are written to sf vector", {
  task = tsk("leipzig")
  learner = lrn("classif.rpart")
  learner$train(task)

  vector = sf::st_read(system.file("extdata", "leipzig_points.gpkg", package = "mlr3spatial"), stringsAsFactors = TRUE, quiet = TRUE)
  vector$land_cover = NULL
  task_predict = as_task_unsupervised(vector)
  pred = predict_spatial(task_predict, learner)
  expect_class(pred, "sf")
  expect_equal(nrow(pred), 97)
  expect_named(pred, c("land_cover", "geometry"))
  expect_class(pred$geometry, "sfc")
})

test_that("probability predictions are written to sf vector", {
  task = tsk("leipzig")
  learner = lrn("classif.rpart", predict_type = "prob")
  learner$train(task)

  vector = sf::st_read(system.file("extdata", "leipzig_points.gpkg", package = "mlr3spatial"), stringsAsFactors = TRUE, quiet = TRUE)
  vector$land_cover = NULL
  task_predict = as_task_unsupervised(vector)
  pred = predict_spatial(task_predict, learner)
  expect_class(pred, "sf")
  expect_equal(nrow(pred), 97)
  expect_named(pred, c(task$class_names, "geometry"))
  expect_true(all(sf::st_drop_geometry(pred) >= 0 & sf::st_drop_geometry(pred) <= 1))
})
