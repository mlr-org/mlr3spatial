test_that("DataBackendStars works", {
  tif = system.file("tif/L7_ETMs.tif", package = "stars")
  l7data = stars::read_stars(tif)

  backend = DataBackendStars$new(l7data)

  # head
  data = backend$head(10L)
  expect_data_table(data, nrows = 10L, ncols = 9L)
  expect_names(names(data),
    identical.to = c("x", "y", "X1", "X2", "X3", "X4", "X5", "X6", "..row_id"))

  # distinct
  expect_equal(backend$distinct(rows = 1:5, cols = "X3"), list("X3" = c(46, 49, 45, 35, 44)))
  data = backend$distinct(rows = 100000:200000, cols = c("X3", "X4"))
  expect_names(names(data), identical.to = c("X3", "X4"))
  expect_numeric(data$X3)
})
