test_that("as_task_regr works on sf objects", {
  skip_if_not_installed("sf")
  requireNamespace("sf", quietly = TRUE)

  x = list(sf::st_point(c(1, 2)), sf::st_point(c(2, 1)))
  geometry = sf::st_sfc(x)
  vector = sf::st_sf(geometry)
  vector[, "y"] = c(1, 2)

  expect_class(as_task_regr(vector, target = "y"), "TaskRegr")
})

test_that("as_task_regr works on stars objects", {
  skip_if_not_installed("stars")
  requireNamespace("stars", quietly = TRUE)

  stack = generate_stars()
  expect_class(as_task_regr(stack, target = "band1"), "TaskRegr")
})

test_that("as_task_regr works on SpatRaster objects", {
  stack = demo_stack_spatraster(size = 1)
  expect_class(as_task_regr(stack, target = "y"), "TaskRegr")
})

test_that("as_task_regr works on RasterBrick objects", {
  skip_if_not_installed("raster")
  requireNamespace("raster", quietly = TRUE)

  stack = demo_stack_rasterbrick(size = 1)
  expect_class(as_task_regr(stack, target = "y"), "TaskRegr")
})

test_that("as_task_regr works on RasterLayer objects", {
  skip_if_not_installed("raster")
  requireNamespace("raster", quietly = TRUE)

  stack = demo_stack_rasterbrick(size = 1)[[5]]
  expect_class(as_task_regr(stack, target = "y"), "TaskRegr")
})
