# DataBackendRaster ------------------------------------------------------------

test_that("DataBackendRaster works with a single numeric layer", {
  stack = create_stack(list(
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

  # data
  expect_data_table(backend$data(rows = seq(100), cols = "x_1"), nrows = 100, ncols = 1, col.names = "strict", types = "numeric") # block read
  expect_names(names(backend$data(rows = seq(100), cols = "x_1")), identical.to = "x_1")
  expect_data_table(backend$data(rows = seq(50), cols = "x_1"), nrows = 50, ncol = 1, col.names = "strict", types = "numeric") # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = "x_1"), nrows = 3, ncol = 1, col.names = "strict", types = "numeric") # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = "x_1")), identical.to = "x_1")

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncol = 1, types = "numeric")
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
  expect_equal(backend$missings(rows = seq(100), cols = "x_1"), c("x_1" = 0))  # fast query
  expect_numeric(backend$missings(rows = seq(10), cols = "x_2"), len = 0)
})

test_that("DataBackendRaster works with a single factor layer", {
  stack = create_stack(list(
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
  expect_data_table(backend$data(rows = seq(50), cols = "c_1"), nrows = 50, ncol = 1, col.names = "strict", types = "factor") # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = "c_1"), nrows = 3, ncol = 1, col.names = "strict", types = "factor") # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = "c_1")), identical.to = "c_1")

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncol = 1, types = "factor")
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
  expect_equal(backend$missings(rows = seq(100), cols = "c_1"), c("c_1" = 0))  # fast query
  expect_numeric(backend$missings(rows = seq(10), cols = "c_2"), len = 0)
})

test_that("DataBackendRaster works with a numeric and a factor layer", {
  stack = create_stack(list(
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
  expect_data_table(backend$data(rows = seq(50), cols = c("x_1", "c_1")), nrows = 50, ncol = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1")), nrows = 3, ncol = 2, col.names = "strict", types = c("numeric", "factor")) # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2")), nrows = 100, ncol = 2, col.names = "strict", types = c("numeric", "factor")) # ignore cols
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2"))), identical.to = c("x_1", "c_1"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncol = 2, types = c("numeric", "factor"))
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

  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0))  # fast query

  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1", "c_2")), c("x_1" = 0, "c_1" = 0))
})

test_that("DataBackendRaster works with multiple numeric and factor layers", {
  stack = create_stack(list(
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

  expect_data_table(backend$data(rows = seq(50), cols = c("x_1", "x_2", "c_1", "c_2")), nrows = 50, ncol = 4, col.names = "strict", types = c("numeric", "factor")) # block read

  expect_data_table(backend$data(rows = c(1, 50, 100), cols = c("x_1", "x_2", "c_1", "c_2")), nrows = 3, ncol = 4, col.names = "strict", types = c("numeric", "factor")) # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = c("x_1", "x_2", "c_1", "c_2"))), identical.to = c("x_1", "x_2", "c_1", "c_2"))

  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2")), nrows = 100, ncol = 4, col.names = "strict", types = c("numeric", "factor")) # ignore cols
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2"))), identical.to = c("x_1", "x_2", "c_1", "c_2"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncol = 4, types = c("numeric", "factor"))
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
  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "x_2", "c_1", "c_2")), c("x_1" = 0, "x_2" = 0, "c_1" = 0, "c_2" = 0))  # fast query
  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "x_2", "c_1", "c_2", "c_3")), c("x_1" = 0, "x_2" = 0, "c_1" = 0, "c_2" = 0))
})

test_that("DataBackendRaster works with a classif train task", {
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10)
  vector = create_vector(stack, n = 10)
  task = as_task_classif(vector, id = "test_vector", target = "y")

  expect_error(DataBackendRaster$new(stack, task), "is already a layer")
  stack$y = NULL

  backend = DataBackendRaster$new(stack, task)

  # backend
  expect_class(backend, "DataBackendRaster")

  # active fields
  expect_names(backend$colnames, identical.to = c("x_1", "y"))
  expect_equal(backend$ncol, 2L)
  expect_equal(backend$rownames, seq(100))
  expect_equal(backend$nrow, 100L)
  expect_class(backend$stack, "SpatRaster")

   # stack
  expect_names(names(backend$stack), identical.to = "x_1")

  # data
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "y")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_factor(backend$data(rows = seq(100), cols = c("x_1", "y"))$y, len = 100, levels = c("a", "b"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncol = 2, types = c("numeric", "factor"))
  expect_names(names(backend$head(n = 10L)), identical.to = c("x_1", "y"))

  # distinct
  expect_list(backend$distinct(rows = seq(100), cols = c("x_1", "y")), len = 2, names = "strict")
  expect_factor(backend$distinct(rows = seq(100), cols = c("x_1", "y"))$y, len = 1, levels = c("a", "b"))
  expect_numeric(backend$distinct(rows = seq(100), cols = c("x_1", "y"))$x_1)

  # missings
  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "y")), c("x_1" = 0, "y" = 100))
  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "y")), c("x_1" = 0, "y" = 10))
})

test_that("DataBackendRaster works with a regr train task", {
  stack = create_stack(list(
    factor_layer("c_1", levels = c("a", "b")),
    numeric_layer("y")),
  dimension = 10)
  vector = create_vector(stack, n = 10)
  task = as_task_regr(vector, id = "test_vector", target = "y")

  expect_error(DataBackendRaster$new(stack, task), "Target of training task is already a layer in the stack")
  stack$y = NULL

  backend = DataBackendRaster$new(stack, task)

  # backend
  expect_class(backend, "DataBackendRaster")

  # active fields
  expect_names(backend$colnames, identical.to = c("c_1", "y"))
  expect_equal(backend$ncol, 2L)
  expect_equal(backend$rownames, seq(100))
  expect_equal(backend$nrow, 100L)
  expect_class(backend$stack, "SpatRaster")

   # stack
  expect_names(names(backend$stack), identical.to = "c_1")

  # data
  expect_data_table(backend$data(rows = seq(100), cols = c("c_1", "y")), nrows = 100, ncols = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_numeric(backend$data(rows = seq(100), cols = c("c_1", "y"))$y, len = 100)

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncol = 2, types = c("numeric", "factor"))
  expect_names(names(backend$head(n = 10L)), identical.to = c("c_1", "y"))

  # distinct
  expect_list(backend$distinct(rows = seq(100), cols = c("c_1", "y")), len = 2, names = "strict")
  expect_numeric(backend$distinct(rows = seq(100), cols = c("c_1", "y"))$y, len = 1)
  expect_character(backend$distinct(rows = seq(100), cols = c("c_1", "y"))$c_1, len = 2)

  # missings
  expect_equal(backend$missings(rows = seq(100), cols = c("c_1", "y")), c("c_1" = 0, "y" = 100))
  expect_equal(backend$missings(rows = seq(10), cols = c("c_1", "y")), c("c_1" = 0, "y" = 10))
})

test_that("data access works", {
  # data
  # [01] [02] [03] [04]
  # [05] [06] [07] [08]
  # [09] [10] [11] [12]
  stack = terra::rast(nrows = 3, ncols = 4)
  stack[] = 1:12
  names(stack) = "y"
  backend = DataBackendRaster$new(stack)

  # [x] [x] [x] [x]
  # [ ] [ ] [ ] [ ]
  # [ ] [ ] [ ] [ ]
  expect_equal(backend$data(rows = 1:4, cols = "y"), data.table(y = c(1, 2, 3, 4)))

  # [ ] [ ] [ ] [ ]
  # [ ] [ ] [ ] [ ]
  # [x] [x] [x] [x]
  expect_equal(backend$data(rows = 9:12, cols = "y"), data.table(y = c(9, 10, 11, 12)))

  # [ ] [ ] [ ] [ ]
  # [ ] [ ] [ ] [x]
  # [x] [x] [ ] [ ]
  expect_equal(backend$data(rows = 8:10, cols = "y"), data.table(y = c(8, 9, 10)))

  # [x] [ ] [x] [ ]
  # [ ] [ ] [ ] [x]
  # [ ] [x] [x] [ ]
  expect_equal(backend$data(rows = c(1, 3, 8, 10, 11), cols = "y"), data.table(y = c(1, 3, 8, 10, 11)))
})

test_that("data prototyp works", {
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10,
  )
  backend = DataBackendRaster$new(stack)

  expect_data_table(backend$data(rows = integer(0), cols = c("x_1", "y")), nrows = 0, ncols = 2)

  # fake response
  stack = create_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10)
  vector = create_vector(stack, n = 10)
  task = as_task_classif(vector, id = "test_vector", target = "y")

  expect_error(DataBackendRaster$new(stack, task), "Target of training task is already a layer in the stack")
  stack$y = NULL

  backend = DataBackendRaster$new(stack, task)

  expect_data_table(backend$data(rows = integer(0), cols = c("x_1", "y")), nrows = 0, ncols = 2)
})

test_that("in memory rasters work", {
  stack = create_stack(list(
    numeric_layer("x_1", in_memory = TRUE),
    factor_layer("c_1", levels = c("a", "b"), in_memory = TRUE)),
  dimension = 10,
  )

  backend = DataBackendRaster$new(stack)

  # backend
  expect_class(backend, "DataBackendRaster")
  expect_equal(backend$stack@ptr$inMemory, c(FALSE, FALSE))
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
  expect_data_table(backend$data(rows = seq(50), cols = c("x_1", "c_1")), nrows = 50, ncol = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1")), nrows = 3, ncol = 2, col.names = "strict", types = c("numeric", "factor")) # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2")), nrows = 100, ncol = 2, col.names = "strict", types = c("numeric", "factor")) # ignore cols
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2"))), identical.to = c("x_1", "c_1"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncol = 2, types = c("numeric", "factor"))
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

  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0))  # fast query

  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1", "c_2")), c("x_1" = 0, "c_1" = 0))
})

test_that("in memory and disk rasters work", {
  stack = create_stack(list(
    numeric_layer("x_1", in_memory = TRUE),
    factor_layer("c_1", levels = c("a", "b"))),
  dimension = 10,
  )

  backend = DataBackendRaster$new(stack)

  # backend
  expect_class(backend, "DataBackendRaster")
  expect_equal(backend$stack@ptr$inMemory, c(FALSE, FALSE))
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
  expect_data_table(backend$data(rows = seq(50), cols = c("x_1", "c_1")), nrows = 50, ncol = 2, col.names = "strict", types = c("numeric", "factor")) # block read
  expect_data_table(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1")), nrows = 3, ncol = 2, col.names = "strict", types = c("numeric", "factor")) # cell read
  expect_names(names(backend$data(rows = c(1, 50, 100), cols = c("x_1", "c_1"))), identical.to = c("x_1", "c_1"))
  expect_data_table(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2")), nrows = 100, ncol = 2, col.names = "strict", types = c("numeric", "factor")) # ignore cols
  expect_names(names(backend$data(rows = seq(100), cols = c("x_1", "c_1", "c_2"))), identical.to = c("x_1", "c_1"))

  # head
  expect_data_table(backend$head(n = 10L), nrows = 10, ncol = 2, types = c("numeric", "factor"))
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

  expect_equal(backend$missings(rows = seq(100), cols = c("x_1", "c_1")), c("x_1" = 0, "c_1" = 0))  # fast query

  expect_equal(backend$missings(rows = seq(10), cols = c("x_1", "c_1", "c_2")), c("x_1" = 0, "c_1" = 0))
})

# stars input ------------------------------------------------------------------

test_that("DataBackendRaster + stars", {
  backend = as_data_backend(generate_stars())

  # head
  data = backend$head(10L)
  expect_data_table(data, nrows = 10L, ncols = 6L)
  expect_names(names(data), identical.to = c("band1", "band2", "band3", "band4", "band5", "band6"))

  # distinct
  expect_equal(backend$distinct(rows = 1:10, cols = "band1"),
    list("band1" = c(69, 63, 60, 61, 62, 64)))
  data = backend$distinct(rows = 1:5, cols = c("band1", "band2"))
  expect_names(names(data), identical.to = c("band1", "band2"))
  expect_numeric(data$band1)

})

# brick input ------------------------------------------------------------------

test_that("DataBackendRaster + raster", {
  backend = as_data_backend(generate_raster_brick())

  # head
  data = backend$head(10L)
  expect_data_table(data, nrows = 10L, ncols = 5L)
  expect_names(names(data), identical.to = c("x_1", "x_2", "x_3", "x_4", "y"))

  # distinct
  # no support for factors when using bricks
  expect_equal(backend$distinct(rows = 1:1000, cols = "y"),
    list("y" = c(1, 0)))
  data = backend$distinct(rows = 1:5, cols = c("y", "x_2"))
  expect_names(names(data), identical.to = c("y", "x_2"))
  expect_numeric(data$y)

})

# raster input -----------------------------------------------------------------

test_that("DataBackendRaster + raster", {
  backend = as_data_backend(generate_raster_brick()[[5]])

  # head
  data = backend$head(10L)
  expect_data_table(data, nrows = 10L, ncols = 1L)
  expect_names(names(data), identical.to = "y")

  # distinct
  # no support for factors when using bricks
  expect_equal(backend$distinct(rows = 1:1000, cols = "y"),
    list("y" = c(1, 0)))
  data = backend$distinct(rows = 1:5, cols = "y")
  expect_names(names(data), identical.to = "y")
  expect_numeric(data$y)

})
