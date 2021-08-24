test_that("as_spatraster_backend from spatraster", {
  stack_classif = demo_stack_spatraster(size = 1, layers = 5)
  value = data.table(ID = c(0, 1), y = c("negative", "positive"))
  terra::setCats(stack_classif, layer = "y", value = value)
  colnames = c(names(stack_classif), "..row_id")
  backend = as_spatraster_backend(stack_classif)

  # head
  data = backend$head(10L)
  expect_data_table(data, nrows = 10L, ncols = 6L)
  expect_names(names(data), identical.to = colnames)

  # distinct
  expect_equal(backend$distinct(rows = NULL, cols = "y"), list(y = c("negative", "positive")))
  data = backend$distinct(rows = 30000:40000, cols = c("x_1", "y"))
  expect_names(names(data), identical.to = c("x_1", "y"))
  expect_numeric(data$x_1)
  expect_factor(as.factor(data$y), levels = c("negative", "positive"))

  # colnames
  expect_equal(backend$colnames, c("x_1", "x_2", "x_3", "x_4", "y", "..row_id"))

  # nrow
  expect_equal(backend$nrow, 49729)

  # ncol
  expect_equal(backend$ncol, 6)

  # stack
  expect_class(backend$stack, "SpatRaster")

  # data
  # [01] [02] [03] [04]
  # [05] [06] [07] [08]
  # [09] [10] [11] [12]
  spatraster = terra::rast(nrow = 3, ncol = 4)
  spatraster[] = 1:12
  names(spatraster) = "y"
  backend = DataBackendSpatRaster$new(spatraster)

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
  # [ ] [ ] [x] [ ]
  expect_equal(backend$data(rows = c(1, 3, 8, 11), cols = "y"), data.table(y = c(1, 3, 8, 11)))
})
