# SpatRaster -------------------------------------------------------------------

test_that("sequential execution works", {
  stack_classif = demo_stack(size = 5, layers = 5)
  backend = DataBackendSpatRaster$new(stack_classif)
  task = as_task_classif(backend, target = "y", positive = "TRUE")
  # train
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  pred = predict_spatial_newdata(learner, stack_classif)
  expect_r6(pred, "Prediction")
})

test_that("parallelization works", {
  stack_classif = demo_stack(size = 5, layers = 5)
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
  stack_classif = demo_stack(size = 5, layers = 5)
  backend = DataBackendSpatRaster$new(stack_classif)
  task = as_task_classif(backend, target = "y", positive = "TRUE")
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))
  pred = predict_spatial_newdata(learner, stack_classif, filename = "foo.tif")
  expect_file("foo.tif")

  expect_error(predict_spatial_newdata(learner, stack_classif, filename = "foo.tif"))

  unlink("foo.tif")
})
