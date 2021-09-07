sf_data = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

test_that("DataBackendVector construction", {

  b = DataBackendVector$new(sf_data)
  expect_backend(b)

  b = as_data_backend(sf_data)

  expect_backend(b)

  sf_data$NAME[21:30] = NA
  b = as_data_backend(sf_data)
  x = b$missings(b$rownames, c("NAME", "NWBIR79"))
  expect_equal(x, set_names(c(10L, 0L), c("NAME", "NWBIR79")))

  b = as_data_backend(sf_data, primary_key = 201:300)
  expect_equal(b$rownames, 201:300)
})

test_that("DataBackendVector with 0 rows", {
  b = as_data_backend(sf_data[integer(), ])
  expect_backend(b)
})

test_that("$missing works", {

  sf_data[c(1, 2), "AREA"] = NA
  b = DataBackendVector$new(sf_data)

  expect_integer(b$missings(rows = 1:10, "AREA"), names = "named", lower = 2, upper = 2)
  expect_integer(b$missings(rows = 1:10, "PERIMETER"), names = "named", lower = 0, upper = 0)

})
