# stack_classif = demo_stack(size = 1000)
# value = data.table(ID = c(0, 1), y = c("negative", "positive"))
# terra::setCats(stack_classif, layer = "y", value = value)
# colnames = names(stack_classif)
# terra::writeRaster(stack_classif, "demo.tif", overwrite = TRUE)

test_that("parallelization works", {
  stack_classif = terra::rast("demo.tif")
  backend = DataBackendSpatial$new(stack_classif)
  task = as_task_classif(backend, target = "y", positive = "positive")
  # train
  learner = lrn("classif.featureless")
  learner$train(task, row_ids = sample(1:task$nrow, 50))

  # parallel
  learner$parallel_predict = TRUE
  with_future("multisession", workers = 2, {
    pred = predict_raster(task, learner, chunksize = 10000L)
    expect_class(pred, "SpatRaster")
  })
})
