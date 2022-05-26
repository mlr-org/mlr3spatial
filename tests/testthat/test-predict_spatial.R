# DataBackendRaster ------------------------------------------------------------

test_that("sequential execution works", {
  task = generate_raster_task()
  learner = lrn("classif.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)

  pred = predict_spatial(task, learner)

  expect_class(pred, "SpatRaster")
})

test_that("output format: stars", {
  skip_if_not_installed("stars")

  task = generate_raster_task()
  learner = lrn("classif.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)

  pred = predict_spatial(task, learner, format = "stars")
  expect_class(pred, "stars")
})

test_that("output format: raster", {
  skip_if_not_installed("raster")
  library(raster)

  task = generate_raster_task()
  learner = lrn("classif.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)

  pred = predict_spatial(task, learner, format = "raster")
  expect_class(pred, "Raster")
})

test_that("parallelization (multicore) works", {
  skip_on_os("windows")
  # parallel
  task = generate_raster_task()
  learner = lrn("classif.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)
  learner$parallel_predict = TRUE
  with_future("multicore", workers = 2, {
    pred = predict_spatial(task, learner, chunksize = 2000L)
    expect_class(pred, "SpatRaster")
  })
})

test_that("parallelization (multisession) works", {
  # parallel
  task = generate_raster_task()
  learner = lrn("classif.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)
  learner$parallel_predict = TRUE
  with_future("multisession", workers = 2, {
    pred = predict_spatial(task, learner)
    expect_class(pred, "SpatRaster")
  })
})

test_that("parallelization (callr) works", {
  # parallel
  task = generate_raster_task()
  learner = lrn("classif.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)
  learner$parallel_predict = TRUE
  with_future(future.callr::callr, workers = 4, {
    pred = predict_spatial(task, learner, chunksize = 2000L)
    expect_class(pred, "SpatRaster")
  })
})

test_that("classif prediction with missing values works",{
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  # train task
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10)
  vector = create_vector(stack, n = 10)
  task_train = as_task_classif(vector, id = "test_vector", target = "y")
  learner = lrn("classif.ranger")
  learner$train(task_train)

  # predict task
  stack$y = NULL
  stack = add_aoi(stack)
  backend = DataBackendRaster$new(stack, task_train)
  task_predict = as_task_classif(backend, id = "test", target = "y")
  ras = predict_spatial(task_predict, learner)

  expect_class(ras, "SpatRaster")
  expect_true(all(is.na(terra::values(ras[["y"]])[seq(10)])))
  expect_numeric(terra::values(ras[["y"]])[11:100], any.missing = FALSE, all.missing = FALSE)
})

test_that("regr prediction with missing values works",{
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  # train task
  stack = create_stack(list(
     factor_layer("c_1", levels = c("a", "b")),
      numeric_layer("y")),
    dimension = 10)
  vector = create_vector(stack, n = 10)
  task_train = as_task_regr(vector, id = "test_vector", target = "y")
  learner = lrn("regr.ranger")
  learner$train(task_train)

  # predict task
  stack$y = NULL
  stack = add_aoi(stack)
  backend = DataBackendRaster$new(stack, task_train)
  task_predict = as_task_regr(backend, id = "test", target = "y")
  ras = predict_spatial(task_predict, learner)

  expect_class(ras, "SpatRaster")
  expect_true(all(is.na(terra::values(ras[["y"]])[seq(10)])))
  expect_numeric(terra::values(ras[["y"]])[11:100], any.missing = FALSE, all.missing = FALSE)
})

# DataBackendVector ------------------------------------------------------------

test_that("sequential execution works", {
  task = generate_vector_task()
  learner = lrn("regr.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)
  pred = predict_spatial(task, learner)
  expect_class(pred, "sf")
})

test_that("parallelization (multicore) works", {
  skip_on_os("windows")
  # parallel
  task = generate_vector_task()
  learner = lrn("regr.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)
  learner$parallel_predict = TRUE
  with_future("multicore", workers = 2, {
    pred = predict_spatial(task, learner)
    expect_class(pred, "sf")
  })
})

test_that("parallelization (multisession) works", {
  # parallel
  task = generate_vector_task()
  learner = lrn("regr.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)
  learner$parallel_predict = TRUE
  with_future("multisession", workers = 2, {
    pred = predict_spatial(task, learner)
    expect_class(pred, "sf")
  })
})

test_that("parallelization (callr) works", {
  # parallel
  task = generate_vector_task()
  learner = lrn("regr.rpart")
  row_ids = sample(1:task$nrow, 50)
  learner$train(task, row_ids = row_ids)
  learner$parallel_predict = TRUE
  with_future(future.callr::callr, workers = 4, {
    pred = predict_spatial(task, learner)
    expect_class(pred, "sf")
  })
})
