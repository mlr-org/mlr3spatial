test_that("as_task_unsupervised works on stars objects", {
  skip_if_not_installed("stars")
  requireNamespace("stars", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  stack = invoke(stars::st_as_stars, .x = stack, .opts = allow_partial_matching)

  expect_class(as_task_unsupervised(stack), "TaskUnsupervised")
})

test_that("as_task_unsupervised works on SpatRaster objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)

  expect_class(as_task_unsupervised(stack), "TaskUnsupervised")
})

test_that("as_task_unsupervised works on RasterBrick objects", {
  skip_if_not_installed("raster")
  requireNamespace("raster", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10, multi_layer_file = TRUE)
  stack = raster::brick(stack)

  expect_class(as_task_unsupervised(stack), "TaskUnsupervised")
})

test_that("as_task_unsupervised works on RasterStack objects", {
  skip_if_not_installed("raster")
  requireNamespace("raster", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  stack = invoke(raster::stack, x = stack, .opts = allow_partial_matching)
  raster::crs(stack) = "EPSG:4326"

  expect_class(as_task_unsupervised(stack), "TaskUnsupervised")
})

test_that("as_task_unsupervised works on sf objects", {
  vector = sf::read_sf(system.file("extdata", "leipzig_points.gpkg", package = "mlr3spatial"), stringsAsFactors = TRUE)
  vector$land_cover = NULL

  expect_class(as_task_unsupervised(vector), "TaskUnsupervised")
})
