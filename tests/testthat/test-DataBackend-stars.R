test_that("DataBackendStars works", {
  tif = system.file("tif/L7_ETMs.tif", package = "stars")
  l7data = stars::read_stars(tif)

  backend = DataBackendStars$new(l7data, quiet = TRUE)

  # head
  data = backend$head(10L)
  expect_data_table(data, nrows = 10L, ncols = 7L)
  expect_names(names(data),
    identical.to = c("X1", "X2", "X3", "X4", "X5", "X6", "..row_id"))

  # distinct
  expect_equal(backend$distinct(rows = 1:5, cols = "X3"), list("X3" = c(46, 49, 45, 35, 44)))
  data = backend$distinct(rows = 100000:200000, cols = c("X3", "X4"))
  expect_names(names(data), identical.to = c("X3", "X4"))
  expect_numeric(data$X3)

  # colnames
  expect_equal(backend$colnames, c("X1", "X2", "X3", "X4", "X5", "X6", "..row_id"))

  # nrow
  expect_equal(backend$nrow, 122848)

  # ncol
  expect_equal(backend$ncol, 7)

  # stack
  expect_class(backend$stack, "stars")
})

test_that("$missing works", {
  tif = system.file("tif/L7_ETMs.tif", package = "stars")
  l7data = stars::read_stars(tif)
  l7data_df = split(l7data, "band")
  l7data_df$X1[1, 1] = NA
  l7data_df$X1[3, 1] = NA
  l7data_na = merge(l7data_df, name = "band")

  backend = DataBackendStars$new(l7data_na, quiet = TRUE)

  expect_integer(backend$missings(rows = 1:10, "X1"), names = "named", lower = 2, upper = 2)
  expect_integer(backend$missings(rows = 1:10, "X2"), names = "named", lower = 0, upper = 0)

})
