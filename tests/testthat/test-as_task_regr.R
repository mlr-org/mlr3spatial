test_that("as_task_regr works on sf objects", {
  skip_if_not_installed("sf")
  requireNamespace("sf", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  vector = sf::st_as_sf(sample_stack(stack))

  expect_class(as_task_regr(vector, target = "y"), "TaskRegr")
})

test_that("as_task_regr works on stars objects", {
  skip_if_not_installed("stars")
  requireNamespace("stars", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  stack = stars::st_as_stars(stack)

  expect_class(as_task_regr(stack, target = "y"), "TaskRegr")
})

test_that("as_task_regr works on SpatRaster objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)

  expect_class(as_task_regr(stack, target = "y"), "TaskRegr")
})

test_that("as_task_regr works on RasterBrick objects", {
  skip_if_not_installed("raster")
  requireNamespace("raster", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  stack = raster::brick(stack)

  expect_class(as_task_regr(stack, target = "y"), "TaskRegr")
})
