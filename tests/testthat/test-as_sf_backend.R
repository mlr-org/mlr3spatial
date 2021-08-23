test_that("as_sf_backend from sf", {
  shp = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  backend = as_sf_backend(shp)

  expect_class(backend$coordinates, "sfc")
  expect_class(backend, "DataBackendSf")
  expect_data_table(backend$head(), nrows = 6, ncols = 15)
})

test_that("as_sf_backend from SpatRaster", {
  stack = demo_stack_spatraster(size = 1, layers = 5)
  backend = as_sf_backend(stack)

  expect_class(backend$coordinates, "sfc_POLYGON")
  expect_class(backend, "DataBackendSf")
  expect_data_table(backend$head(), nrows = 6, ncols = 6)
  expect_names(colnames(backend$head()),
    identical.to = c("x_1", "x_2", "x_3", "x_4", "y", "..row_id"))
})

test_that("as_sf_backend from RasterBrick", {
  stack = demo_stack_rasterbrick(size = 1, layers = 5)
  backend = as_sf_backend(stack)

  expect_class(backend$coordinates, "sfc_POLYGON")
  expect_class(backend, "DataBackendSf")
  expect_data_table(backend$head(), nrows = 6, ncols = 6)
  expect_names(colnames(backend$head()),
    identical.to = c("x_1", "x_2", "x_3", "x_4", "y", "..row_id"))
})

test_that("as_sf_backend from DataBackendSpatRaster", {
  stack = demo_stack_spatraster(size = 1, layers = 5)
  backend_spatraster = DataBackendSpatRaster$new(stack)
  backend = as_sf_backend(backend_spatraster)

  expect_class(backend$coordinates, "sfc")
  expect_class(backend, "DataBackendSf")
  expect_data_table(backend$head(), nrows = 6, ncols = 6)
  expect_names(colnames(backend$head()),
    identical.to = c("x_1", "x_2", "x_3", "x_4", "y", "..row_id"))

  backend = as_sf_backend(backend_spatraster, polygons = TRUE)
  expect_class(backend$coordinates, "sfc_POLYGON")
})

test_that("as_sf_backend from DataBackendRasterBrick", {
  stack = demo_stack_rasterbrick(size = 1, layers = 5)
  backend_rasterbrick = DataBackendRasterBrick$new(stack, response = "y")
  backend = as_sf_backend(backend_rasterbrick)

  expect_class(backend$coordinates, "sfc")
  expect_class(backend, "DataBackendSf")
  expect_data_table(backend$head(), nrows = 6, ncols = 6)
  expect_names(colnames(backend$head()),
    identical.to = c("x_1", "x_2", "x_3", "x_4", "y", "..row_id"))
})
