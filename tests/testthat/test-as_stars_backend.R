test_that("as_stars_backend from stars", {
  tif = system.file("tif/L7_ETMs.tif", package = "stars")
  l7data = stars::read_stars(tif)
  backend = as_stars_backend(l7data, quiet = TRUE)

  expect_data_table(backend$coordinates, max.cols = 2, min.cols = 2, any.missing = FALSE)
  expect_class(backend, "DataBackendStars")
  expect_data_table(backend$head(), nrows = 6, ncols = 7)
  expect_names(colnames(backend$head()),
    identical.to = c("X1", "X2", "X3", "X4", "X5", "X6", "..row_id"))
})

test_that("as_stars_backend from SpatRaster", {
  stack = demo_stack_spatraster(size = 1, layers = 5)
  backend = as_stars_backend(stack, quiet = TRUE)

  expect_data_table(backend$coordinates, max.cols = 2, min.cols = 2, any.missing = FALSE)
  expect_class(backend, "DataBackendStars")
  expect_data_table(backend$head(), nrows = 6, ncols = 6)
  expect_names(colnames(backend$head()),
    identical.to = c("x_1", "x_2", "x_3", "x_4", "y.1", "..row_id"))
})

test_that("as_stars_backend from RasterBrick", {
  stack = demo_stack_rasterbrick(size = 1, layers = 5)
  backend = as_stars_backend(stack, quiet = TRUE)

  expect_data_table(backend$coordinates, max.cols = 2, min.cols = 2, any.missing = FALSE)
  expect_class(backend, "DataBackendStars")
  expect_data_table(backend$head(), nrows = 6, ncols = 6)
  expect_names(colnames(backend$head()),
    identical.to = c("x_1", "x_2", "x_3", "x_4", "y.1", "..row_id"))
})

test_that("as_stars_backend from DataBackendSpatRaster", {
  stack = demo_stack_spatraster(size = 1, layers = 5)
  backend_spatraster = DataBackendSpatRaster$new(stack)
  backend = as_stars_backend(backend_spatraster, quiet = TRUE)

  expect_data_table(backend$coordinates, max.cols = 2, min.cols = 2, any.missing = FALSE)
  expect_class(backend, "DataBackendStars")
  expect_data_table(backend$head(), nrows = 6, ncols = 6)
  expect_names(colnames(backend$head()),
    identical.to = c("x_1", "x_2", "x_3", "x_4", "y.1", "..row_id"))
})

test_that("as_stars_backend from DataBackendRasterBrick", {
  stack = demo_stack_rasterbrick(size = 1, layers = 5)
  backend_rasterbrick = DataBackendRasterBrick$new(stack, response = "y")
  backend = as_stars_backend(backend_rasterbrick, quiet = TRUE)

  expect_data_table(backend$coordinates, max.cols = 2, min.cols = 2, any.missing = FALSE)
  expect_class(backend, "DataBackendStars")
  expect_data_table(backend$head(), nrows = 6, ncols = 6)
  expect_names(colnames(backend$head()),
    identical.to = c("x_1", "x_2", "x_3", "x_4", "y.1", "..row_id"))
})
