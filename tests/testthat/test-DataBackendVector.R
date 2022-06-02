# DataBackendVector methods ----------------------------------------------------

test_that("DataBackendVector works with a numeric and a factor layer", {
  skip_if_not_installed("sf")
  requireNamespace("sf", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("c_1", levels = c("a", "b"))),
  dimension = 10,
  )
  vector = sample_stack(stack, n = 100)
  vector[["..row_id"]] = seq_row(vector)

  backend = DataBackendVector$new(vector, primary_key = "..row_id")

  # backend
  expect_class(backend, "DataBackendVector")

  # active fields
  expect_names(backend$colnames, identical.to = c("x_1", "c_1", "..row_id"))
  expect_equal(backend$ncol, 3)
  expect_equal(backend$rownames, seq(100))
  expect_equal(backend$nrow, 100)
  expect_class(backend$geometry, "sfc")

  # data
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(50), cols = c("x_1", "c_1")), nrows = 50, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1")), nrows = 3, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # ignore cols
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2"))), identical.to = c("x_1", "c_1"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncols = 3, types = c("numeric", "factor"))
  expect_names(names(backend$head(n = 10L)), identical.to = c("x_1", "c_1", "..row_id"))

  # distinct
  expect_list(backend$distinct(rows = 1, cols = c("x_1", "c_1")), len = 2, names = "strict")
  expect_string(backend$distinct(rows = 1, cols = c("x_1", "c_1"))$c_1)
  expect_numeric(backend$distinct(rows = 1, cols = c("x_1", "c_1"))$x_1, len = 1)

  expect_list(backend$distinct(rows = seq(20), cols = c("x_1", "c_1")), len = 2, names = "strict")
  expect_names(backend$distinct(rows = seq(20), cols = c("x_1", "c_1"))$c_1, must.include = c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(20), cols = c("x_1", "c_1"))$x_1)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "c_1")), len = 2, names = "strict")
  expect_names(backend$distinct(rows = seq(100), cols = c("x_1", "c_1"))$c_1, must.include = c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "c_1"))$x_1)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2")), len = 2, names = "strict")
  expect_names(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2"))$c_1, must.include = c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2"))$x_1)

  # missings
  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0))

  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0))

  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1", "c_2")), c("x_1" = 0, "c_1" = 0))
})

# as_data_backend input formats ------------------------------------------------

test_that("DataBackendVector works with renamed geometry column", {
  skip_if_not_installed("sf")
  requireNamespace("sf", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("c_1", levels = c("a", "b"))),
  dimension = 10)
  vector = sample_stack(stack, n = 100)
  sf::st_geometry(vector) = "geom"

  backend = as_data_backend(vector)
  expect_class(backend, "DataBackendVector")
  expect_class(backend$geometry, "sfc")
})

test_that("as_data_backend works on sf objects", {
  skip_if_not_installed("sf")
  requireNamespace("sf", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("c_1", levels = c("a", "b"))),
  dimension = 10)
  vector = sample_stack(stack, n = 100)

  backend = as_data_backend(vector)

  expect_class(backend, "DataBackendVector")
  expect_class(backend$geometry, "sfc")
})
