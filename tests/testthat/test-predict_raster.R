stack_classif = demo_stack(size = 1000)
value = data.table(ID = c(0, 1), y = c("negative", "positive"))
terra::setCats(stack_classif, layer = "y", value = value)
colnames = names(stack_classif)
terra::writeRaster(stack_classif, "demo.tif", overwrite = TRUE)

test_that("predict_raster sequential works", {
  stack_classif = terra::rast("demo.tif")
  pkgload::load_all()
  library(mlr3learners)
  backend = DataBackendSpatial$new(stack_classif)
  task = as_task_classif(backend, target = "y", positive = "positive")
  set.seed(42)
  row_ids = sample(1:task$nrow, 500)

  # train
  learner = lrn("classif.svm")
  learner$train(task, row_ids = row_ids)

  # 151s mit size = 1000
  pred = predict_raster(task, learner, chunksize = 100L)
  expect_class(pred, "SpatRaster")
  foo = terra::values(pred)

  # 124s mit size = 1000
  pred = predict_raster(task, learner, chunksize = 1000L)
  expect_class(pred, "SpatRaster")

  e1071svm = e1071::svm(y ~ ., task$data(rows = row_ids))
  # 101s mit size = 1000
  pred_terra = terra::predict(stack_classif, e1071svm)
  foo_terra = terra::values(pred_terra)
  all.equal(as.numeric(terra::values(pred)), as.numeric(terra::values(pred_terra)))

  # 41s mit size = 1000
  # system.time(terra::predict(stack_classif, e1071svm, cores = 4, cpkgs = "e1071"))

})

test_that("parallelization works", {
  stack_classif = terra::rast("demo.tif")
  pkgload::load_all()
  library(mlr3learners)
  backend = DataBackendSpatial$new(stack_classif)
  task = as_task_classif(backend, target = "y", positive = "positive")
  set.seed(42)
  row_ids = sample(1:task$nrow, 500)
  # train
  learner = lrn("classif.svm")
  learner$train(task, row_ids = row_ids)

  # parallel
  learner$parallel_predict = TRUE
  # 42s mit size = 1000
  # multicore works, multisession throws external pointer failure
  system.time(with_future("multicore", workers = 2, {
    pred = predict_raster(task, learner, chunksize = 1000L)
    expect_class(pred, "SpatRaster")
  }))
})

learner$parallel_predict = TRUE
system.time(with_future("multisession", workers = 4, {
  learner$predict(task, row_ids = 1:task$nrow)
}))
