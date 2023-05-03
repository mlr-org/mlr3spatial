# DataBackendRaster methods ----------------------------------------------------

test_that("DataBackendRaster works with a single numeric layer", {
  stack = generate_stack(list(
    numeric_layer("x_1")),
  dimension = 10,
  )

  backend = DataBackendRaster$new(stack)

  # backend
  expect_class(backend, "DataBackendRaster")

  # active fields
  expect_names(backend$colnames, identical.to = "x_1")
  expect_equal(backend$ncol, 1L)
  expect_equal(backend$rownames, seq(100))
  expect_equal(backend$nrow, 100L)
  expect_class(backend$stack, "SpatRaster")

  # stack
  expect_names(names(backend$stack), identical.to = "x_1")

  # crs
  expect_length(terra::crs(backend$stack, describe = TRUE), 5L)

  # data
  expect_data_table(backend$data(rows = seq(100), cols = "x_1"), nrows = 100, ncols = 1, col.names = "strict", types = "numeric") # block read
  expect_names(names(backend$data(rows = seq(100), cols = "x_1")), identical.to = "x_1")
  expect_data_table(backend$data(rows = seq(50), cols = "x_1"), nrows = 50, ncols = 1, col.names = "strict", types = "numeric") # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = "x_1"), nrows = 3, ncols = 1, col.names = "strict", types = "numeric") # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = "x_1")), identical.to = "x_1")

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncols = 1, types = "numeric")
  expect_names(names(backend$head(n = 10L)), identical.to = "x_1")

  # distinct
  expect_list(backend$distinct(rows = 1, cols = "x_1"), len = 1, names = "strict") # slow query
  expect_numeric(backend$distinct(rows = 1, cols = "x_1")$x_1, len = 1)

  expect_list(backend$distinct(rows = seq(10), cols = "x_1"), len = 1, names = "strict") # slow query
  expect_numeric(backend$distinct(rows = seq(10), cols = "x_1")$x_1)

  expect_list(backend$distinct(rows = seq(100), cols = "x_1"), len = 1, names = "strict") # fast query
  expect_numeric(backend$distinct(rows = seq(100), cols = "x_1")$x_1)

  expect_list(backend$distinct(rows = seq(10), cols = "x_2"), len = 0, names = "strict")

  # missings
  expect_equal(backend$missings(rows = seq(10), cols = "x_1"), c("x_1" = 0)) # slow query
  expect_equal(backend$missings(rows = seq(100), cols = "x_1"), c("x_1" = 0)) # fast query
  expect_numeric(backend$missings(rows = seq(10), cols = "x_2"), len = 0)
})

test_that("DataBackendRaster works with a single factor layer", {
  stack = generate_stack(list(
    factor_layer("c_1", levels = c("a", "b"))),
  dimension = 10,
  )

  backend = DataBackendRaster$new(stack)

  # backend
  expect_class(backend, "DataBackendRaster")

  # active fields
  expect_names(backend$colnames, identical.to = "c_1")
  expect_equal(backend$ncol, 1L)
  expect_equal(backend$rownames, seq(100))
  expect_equal(backend$nrow, 100L)
  expect_class(backend$stack, "SpatRaster")
  expect_names(names(backend$stack), identical.to = "c_1")

  # data
  expect_data_table(backend$data(rows = seq(100), cols = "c_1"), nrows = 100, ncols = 1, col.names = "strict", types = "factor") # block read
  expect_names(names(backend$data(rows = seq(100), cols = "c_1")), identical.to = "c_1")
  expect_data_table(backend$data(rows = seq(50), cols = "c_1"), nrows = 50, ncols = 1, col.names = "strict", types = "factor") # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = "c_1"), nrows = 3, ncols = 1, col.names = "strict", types = "factor") # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = "c_1")), identical.to = "c_1")

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncols = 1, types = "factor")
  expect_names(names(backend$head(n = 10L)), identical.to = "c_1")

  # distinct
  expect_list(backend$distinct(rows = 1, cols = "c_1"), len = 1, names = "strict") # slow query
  expect_equal(backend$distinct(rows = 1, cols = "c_1")$c_1, "a")

  expect_list(backend$distinct(rows = seq(20), cols = "c_1"), len = 1, names = "strict") # slow query
  expect_equal(backend$distinct(rows = seq(20), cols = "c_1")$c_1, c("a", "b"))

  expect_list(backend$distinct(rows = seq(100), cols = "c_1"), len = 1, names = "strict") # fast query
  expect_equal(backend$distinct(rows = seq(100), cols = "c_1")$c_1, c("a", "b"))
  expect_list(backend$distinct(rows = seq(20), cols = "c_2"), len = 0, names = "strict")

  # missings
  expect_equal(backend$missings(rows = seq(10), cols = "c_1"), c("c_1" = 0)) # slow query
  expect_equal(backend$missings(rows = seq(100), cols = "c_1"), c("c_1" = 0)) # fast query
  expect_numeric(backend$missings(rows = seq(10), cols = "c_2"), len = 0)
})

test_that("DataBackendRaster works with a numeric and a factor layer", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("c_1", levels = c("a", "b"))),
  dimension = 10,
  )

  backend = DataBackendRaster$new(stack)

  # backend
  expect_class(backend, "DataBackendRaster")

  # active fields
  expect_names(backend$colnames, identical.to = c("x_1", "c_1"))
  expect_equal(backend$ncol, 2)
  expect_equal(backend$rownames, seq(100))
  expect_equal(backend$nrow, 100)
  expect_class(backend$stack, "SpatRaster")
  expect_names(names(backend$stack), identical.to = c("x_1", "c_1"))

  # data
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(50), cols = c("x_1", "c_1")), nrows = 50, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1")), nrows = 3, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # ignore cols
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2"))), identical.to = c("x_1", "c_1"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncols = 2, types = c("numeric", "factor"))
  expect_names(names(backend$head(n = 10L)), identical.to = c("x_1", "c_1"))

  # distinct
  expect_list(backend$distinct(rows = 1, cols = c("x_1", "c_1")), len = 2, names = "strict") # slow query
  expect_equal(backend$distinct(rows = 1, cols = c("x_1", "c_1"))$c_1, "a")
  expect_numeric(backend$distinct(rows = 1, cols = c("x_1", "c_1"))$x_1, len = 1)

  expect_list(backend$distinct(rows = seq(20), cols = c("x_1", "c_1")), len = 2, names = "strict") # slow query
  expect_equal(backend$distinct(rows = seq(20), cols = c("x_1", "c_1"))$c_1, c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(20), cols = c("x_1", "c_1"))$x_1)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "c_1")), len = 2, names = "strict") # fast query
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "c_1"))$c_1, c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "c_1"))$x_1)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2")), len = 2, names = "strict")
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2"))$c_1, c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2"))$x_1)

  # missings
  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0)) # slow query

  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0)) # fast query

  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1", "c_2")), c("x_1" = 0, "c_1" = 0))

  # task
  expect_class(as_task_unsupervised(backend, id = "test", target = "c_1"), "TaskUnsupervised")
})

test_that("DataBackendRaster works with multiple numeric and factor layers", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("x_2"),
    factor_layer("c_1", levels = c("a", "b")),
    factor_layer("c_2", levels = c("a1", "a2", "a3"))),
  dimension = 10,
  )

  backend = DataBackendRaster$new(stack)

  # backend
  expect_class(backend, "DataBackendRaster")

  # active fields
  expect_names(backend$colnames, identical.to = c("x_1", "x_2", "c_1", "c_2"))
  expect_equal(backend$ncol, 4)
  expect_equal(backend$rownames, seq(100))
  expect_equal(backend$nrow, 100)
  expect_class(backend$stack, "SpatRaster")
  expect_names(names(backend$stack), identical.to = c("x_1", "x_2", "c_1", "c_2"))

  # data
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2")), nrows = 100, ncols = 4, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2"))), identical.to = c("x_1", "x_2", "c_1", "c_2"))

  expect_data_table(backend$data(rows = seq(50), cols = c("x_1", "x_2", "c_1", "c_2")), nrows = 50, ncols = 4, col.names = "strict", types = c("numeric", "factor")) # block read

  expect_data_table(backend$data(rows = c(1, 50, 100), cols = c("x_1", "x_2", "c_1", "c_2")), nrows = 3, ncols = 4, col.names = "strict", types = c("numeric", "factor")) # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = c("x_1", "x_2", "c_1", "c_2"))), identical.to = c("x_1", "x_2", "c_1", "c_2"))

  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2")), nrows = 100, ncols = 4, col.names = "strict", types = c("numeric", "factor")) # ignore cols
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2"))), identical.to = c("x_1", "x_2", "c_1", "c_2"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncols = 4, types = c("numeric", "factor"))
  expect_names(names(backend$head(n = 10L)), identical.to = c("x_1", "x_2", "c_1", "c_2"))

  # distinct
  expect_list(backend$distinct(rows = 1, cols = c("x_1", "x_2", "c_1", "c_2")), len = 4, names = "strict") # slow query
  expect_equal(backend$distinct(rows = 1, cols = c("x_1", "x_2", "c_1", "c_2"))$c_1, "a")
  expect_equal(backend$distinct(rows = 1, cols = c("x_1", "x_2", "c_1", "c_2"))$c_2, "a1")
  expect_numeric(backend$distinct(rows = 1, cols = c("x_1", "x_2", "c_1", "c_2"))$x_1, len = 1)
  expect_numeric(backend$distinct(rows = 1, cols = c("x_1", "x_2", "c_1", "c_2"))$x_2, len = 1)

  expect_list(backend$distinct(rows = seq(20), cols = c("x_1", "x_2", "c_1", "c_2")), len = 4, names = "strict") # slow query
  expect_equal(backend$distinct(rows = seq(20), cols = c("x_1", "x_2", "c_1", "c_2"))$c_1, c("a", "b"))
  expect_equal(backend$distinct(rows = seq(20), cols = c("x_1", "x_2", "c_1", "c_2"))$c_2, c("a1", "a2", "a3"))
  expect_numeric(backend$distinct(rows = seq(20), cols = c("x_1", "x_2", "c_1", "c_2"))$x_1)
  expect_numeric(backend$distinct(rows = seq(20), cols = c("x_1", "x_2", "c_1", "c_2"))$x_2)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2")), len = 4, names = "strict") # fast query
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2"))$c_1, c("a", "b"))
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2"))$c_2, c("a1", "a2", "a3"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2"))$x_1)
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2"))$x_2)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2", "c3")), len = 4, names = "strict")
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2", "c3"))$c_1, c("a", "b"))
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2", "c3"))$c_1, c("a", "b"))
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2", "c3"))$c_2, c("a1", "a2", "a3"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2", "c3"))$x_1)
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2", "c3"))$x_2)

  # missings
  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "x_2", "c_1", "c_2")), c("x_1" = 0, "x_2" = 0, "c_1" = 0, "c_2" = 0)) # slow query
  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2")), c("x_1" = 0, "x_2" = 0, "c_1" = 0, "c_2" = 0)) # fast query
  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "x_2", "c_1", "c_2", "c_3")), c("x_1" = 0, "x_2" = 0, "c_1" = 0, "c_2" = 0))

  # task
  expect_class(as_task_unsupervised(backend, id = "test"), "TaskUnsupervised")
})

test_that("data access works", {
  # data
  # [13] [14] [15] [16]
  # [17] [18] [19] [20]
  # [21] [22] [23] [24]
  stack = terra::rast(nrows = 3, ncols = 4)
  stack[] = 13:24
  names(stack) = "y"
  backend = DataBackendRaster$new(stack)

  # [x] [x] [x] [x]
  # [ ] [ ] [ ] [ ]
  # [ ] [ ] [ ] [ ]
  expect_equal(backend$data(rows = 1:4, cols = "y"), data.table(y = c(13, 14, 15, 16)))

  # [ ] [ ] [ ] [ ]
  # [ ] [ ] [ ] [ ]
  # [x] [x] [x] [x]
  expect_equal(backend$data(rows = 9:12, cols = "y"), data.table(y = c(21, 22, 23, 24)))

  # [ ] [ ] [ ] [ ]
  # [ ] [ ] [ ] [x]
  # [x] [x] [ ] [ ]
  expect_equal(backend$data(rows = 8:10, cols = "y"), data.table(y = c(20, 21, 22)))

  # [x] [ ] [x] [ ]
  # [ ] [ ] [ ] [x]
  # [ ] [x] [x] [ ]
  expect_equal(backend$data(rows = c(1, 3, 8, 10, 11), cols = "y"), data.table(y = c(13, 15, 20, 22, 23)))
})

test_that("data prototyp works", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10,
  )
  backend = DataBackendRaster$new(stack)

  expect_data_table(backend$data(rows = integer(0), cols = c("x_1", "y")), nrows = 0, ncols = 2)
})

test_that("in memory rasters work", {
  stack = generate_stack(list(
    numeric_layer("x_1", in_memory = TRUE),
    factor_layer("c_1", levels = c("a", "b"), in_memory = TRUE)),
  dimension = 10,
  )

  backend = DataBackendRaster$new(stack)

  # backend
  expect_class(backend, "DataBackendRaster")
  expect_equal(terra::inMemory(backend$stack), c(FALSE, FALSE))
  expect_names(names(backend$stack), identical.to = c("x_1", "c_1"))

  # active fields
  expect_names(backend$colnames, identical.to = c("x_1", "c_1"))
  expect_equal(backend$ncol, 2)
  expect_equal(backend$rownames, seq(100))
  expect_equal(backend$nrow, 100)
  expect_class(backend$stack, "SpatRaster")

  # data
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(50), cols = c("x_1", "c_1")), nrows = 50, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1")), nrows = 3, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # ignore cols
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2"))), identical.to = c("x_1", "c_1"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncols = 2, types = c("numeric", "factor"))
  expect_names(names(backend$head(n = 10L)), identical.to = c("x_1", "c_1"))

  # distinct
  expect_list(backend$distinct(rows = 1, cols = c("x_1", "c_1")), len = 2, names = "strict") # slow query
  expect_equal(backend$distinct(rows = 1, cols = c("x_1", "c_1"))$c_1, "a")
  expect_numeric(backend$distinct(rows = 1, cols = c("x_1", "c_1"))$x_1, len = 1)

  expect_list(backend$distinct(rows = seq(20), cols = c("x_1", "c_1")), len = 2, names = "strict") # slow query
  expect_equal(backend$distinct(rows = seq(20), cols = c("x_1", "c_1"))$c_1, c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(20), cols = c("x_1", "c_1"))$x_1)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "c_1")), len = 2, names = "strict") # fast query
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "c_1"))$c_1, c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "c_1"))$x_1)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2")), len = 2, names = "strict")
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2"))$c_1, c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2"))$x_1)

  # missings
  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0)) # slow query

  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0)) # fast query

  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1", "c_2")), c("x_1" = 0, "c_1" = 0))
})

test_that("in memory and disk rasters work", {
  stack = generate_stack(list(
    numeric_layer("x_1", in_memory = TRUE),
    factor_layer("c_1", levels = c("a", "b"))),
  dimension = 10,
  )

  backend = DataBackendRaster$new(stack)

  # backend
  expect_class(backend, "DataBackendRaster")
  expect_equal(terra::inMemory(backend$stack), c(FALSE, FALSE))
  expect_names(names(backend$stack), identical.to = c("x_1", "c_1"))

  # active fields
  expect_names(backend$colnames, identical.to = c("x_1", "c_1"))
  expect_equal(backend$ncol, 2)
  expect_equal(backend$rownames, seq(100))
  expect_equal(backend$nrow, 100)
  expect_class(backend$stack, "SpatRaster")

  # data
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(50), cols = c("x_1", "c_1")), nrows = 50, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1")), nrows = 3, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # ignore cols
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2"))), identical.to = c("x_1", "c_1"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncols = 2, types = c("numeric", "factor"))
  expect_names(names(backend$head(n = 10L)), identical.to = c("x_1", "c_1"))

  # distinct
  expect_list(backend$distinct(rows = 1, cols = c("x_1", "c_1")), len = 2, names = "strict") # slow query
  expect_equal(backend$distinct(rows = 1, cols = c("x_1", "c_1"))$c_1, "a")
  expect_numeric(backend$distinct(rows = 1, cols = c("x_1", "c_1"))$x_1, len = 1)

  expect_list(backend$distinct(rows = seq(20), cols = c("x_1", "c_1")), len = 2, names = "strict") # slow query
  expect_equal(backend$distinct(rows = seq(20), cols = c("x_1", "c_1"))$c_1, c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(20), cols = c("x_1", "c_1"))$x_1)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "c_1")), len = 2, names = "strict") # fast query
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "c_1"))$c_1, c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "c_1"))$x_1)

  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2")), len = 2, names = "strict")
  expect_equal(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2"))$c_1, c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "c_1", "c_2"))$x_1)

  # missings
  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0)) # slow query

  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0)) # fast query

  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1", "c_2")), c("x_1" = 0, "c_1" = 0))
})

# as_data_backend input formats ------------------------------------------------

test_that("as_data_backend works on SpatRaster objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  expect_class(as_data_backend(stack), "DataBackendRaster")
})

test_that("as_data_backend works on stars objects", {
  skip_if_not_installed("stars")
  requireNamespace("stars", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  stack = invoke(stars::st_as_stars, .x = stack, .opts = allow_partial_matching)
  expect_class(as_data_backend(stack), "DataBackendRaster")
})

test_that("as_data_backend works on RasterBrick objects", {
  skip_if_not_installed("raster")
  requireNamespace("raster", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10, multi_layer_file = TRUE)
  stack = invoke(raster::brick, stack, .opts = allow_partial_matching)

  expect_class(as_data_backend(stack), "DataBackendRaster")
})

test_that("as_data_backend works on RasterStack objects", {
  skip_if_not_installed("raster")
  requireNamespace("raster", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 10)
  stack = invoke(raster::stack, x = stack, .opts = allow_partial_matching)
  raster::crs(stack) = "EPSG:4326"

  expect_class(as_data_backend(stack, target = "y"), "DataBackendRaster")
})
