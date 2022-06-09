test_that("as_task works on sf objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  vector = sf::st_as_sf(sample_stack(stack))

  expect_class(as_task(vector, target = "y"), "Task")
})

test_that("as_task works on stars objects", {
  skip_if_not_installed("stars")
  requireNamespace("stars", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  stack = invoke(stars::st_as_stars, .x = stack, .opts = allow_partial_matching)

  expect_class(as_task(stack, target = "y"), "Task")
})

test_that("as_task works on SpatRaster objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)

  expect_class(as_task(stack, target = "y"), "Task")
})

test_that("as_task works on RasterBrick objects", {
  skip_if_not_installed("raster")
  requireNamespace("raster", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10, multi_layer_file = TRUE)
  stack = raster::brick(stack)

  expect_class(as_task(stack, target = "y"), "Task")
})

test_that("as_task works on RasterStack objects", {
  skip_if_not_installed("raster")
  requireNamespace("raster", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  stack = invoke(raster::stack, x = stack, .opts = allow_partial_matching)
  raster::crs(stack) = "EPSG:4326"

  expect_class(as_task(stack, target = "y"), "Task")
})
