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

test_that("NAs in features", {
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10)
  vector = create_vector(stack, n = 10)
  task = as_task_classif(vector, id = "test_vector", target = "y")


  learner = lrn("classif.ranger")
  learner$properties = c(learner$properties, "missings")
  learner_na = PipeOpLearnerNA$new(learner)

  learner_na$train(list(task))


  stack$y = NULL
  stack = add_aoi(stack)
  backend = DataBackendRaster$new(stack, task)
  task_raster = as_task_classif(backend, id = "test", target = "y")

  learner_na$predict(list(task_raster))




  learner$train(task)




  pred = predict_spatial(task_raster, learner)

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
