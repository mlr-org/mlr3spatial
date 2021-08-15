sf_data = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

test_that("DataBackendDataTable construction", {

  b = DataBackendSF$new(sf_data)
  expect_backend(b)

  b = as_sf_backend(sf_data)

  expect_backend(b)

  sf_data$NAME[21:30] = NA
  b = as_data_backend(sf_data)
  x = b$missings(b$rownames, c("NAME", "NWBIR79"))
  expect_equal(x, set_names(c(10L, 0L), c("NAME", "NWBIR79")))

  b = as_sf_backend(sf_data, primary_key = 201:300)
  expect_equal(b$rownames, 201:300)
})

test_that("DataBackendDataTable with 0 rows", {
  b = as_sf_backend(sf_data[integer(), ])
  expect_backend(b)
})
