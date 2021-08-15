test_that("DataBackendRasterBrick works", {
  stack_classif = demo_stack_raster(size = 5, layers = 5)
  # value = data.table(ID = c(0, 1), y = c("negative", "positive"))
  # setValues(stack_classif, c("negative", "positive"), layer = 5)
  # raster::factorValues(stack_classif, layer = 5, v = value)
  colnames = names(stack_classif)

  backend = DataBackendRasterBrick$new(stack_classif)

  # head
  data = backend$head(10L)
  expect_data_table(data, nrow = 10L, ncol = 5L)
  expect_names(names(data), identical.to = colnames)

  # distinct
  expect_equal(backend$distinct(rows = NULL, cols = "y"), list(y = c(0, 1)))
  data = backend$distinct(rows = 1:100, cols = c("x_1", "y"))
  expect_names(names(data), identical.to = c("x_1", "y"))
  expect_numeric(c(data$x_1, data$y))

  # data
  # [01] [02] [03] [04]
  # [05] [06] [07] [08]
  # [09] [10] [11] [12]
  raster = raster::brick(nrow = 3, ncol = 4)
  raster[] = 1:12
  names(raster) = "y"
  backend = DataBackendRasterBrick$new(raster)

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


# FIXME: test as_task_classif
# FIXME: test printer
