# DataBackendRaster ------------------------------------------------------------

test_that("sequential execution works", {
  pred = predict_spatial(task, learner)
  expect_class(pred, "SpatRaster")
})


test_that("output format: stars", {
  pred = predict_spatial(task, learner, format = "stars")
  expect_class(pred, "stars")
})

test_that("output format: raster", {
  pred = predict_spatial(task, learner, format = "raster")
  expect_class(pred, "Raster")
})

test_that("parallelization (multicore) works", {
  skip_on_os("windows")
  # parallel
  learner$parallel_predict = TRUE
  with_future("multicore", workers = 2, {
    pred = predict_spatial(task, learner, chunksize = 2000L)
    expect_class(pred, "SpatRaster")
  })
})

test_that("parallelization (multisession) works", {
  # parallel
  learner$parallel_predict = TRUE
  with_future("multisession", workers = 2, {
    pred = predict_spatial(task, learner)
    expect_class(pred, "SpatRaster")
  })
})

test_that("parallelization (callr) works", {
  # parallel
  learner$parallel_predict = TRUE
  with_future(future.callr::callr, workers = 4, {
    pred = predict_spatial(task, learner, chunksize = 2000L)
    expect_class(pred, "SpatRaster")
  })
})

# DataBackendVector ------------------------------------------------------------

test_that("sequential execution works", {
  pred = predict_spatial(task_vec, learner_regr)
  expect_class(pred, "sf")
})

test_that("parallelization (multicore) works", {
  skip_on_os("windows")
  # parallel
  learner$parallel_predict = TRUE
  with_future("multicore", workers = 2, {
    pred = predict_spatial(task_vec, learner_regr)
    expect_class(pred, "sf")
  })
})

test_that("parallelization (multisession) works", {
  # parallel
  learner$parallel_predict = TRUE
  with_future("multisession", workers = 2, {
    pred = predict_spatial(task_vec, learner_regr)
    expect_class(pred, "sf")
  })
})

test_that("parallelization (callr) works", {
  # parallel
  learner$parallel_predict = TRUE
  with_future(future.callr::callr, workers = 4, {
    pred = predict_spatial(task_vec, learner_regr)
    expect_class(pred, "sf")
  })
})
