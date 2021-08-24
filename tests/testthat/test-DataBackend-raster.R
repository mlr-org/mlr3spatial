test_that("DataBackendRasterBrick works", {
  stack_classif = demo_stack_rasterbrick(size = 1, layers = 5)
  colnames = names(stack_classif)

  backend = DataBackendRasterBrick$new(stack_classif, response = "y")
  backend_classif = DataBackendRasterBrick$new(stack_classif, response = "y",
    response_is_factor = TRUE)

  # head
  data = backend$head(10L)
  expect_data_table(data, nrow = 10L, ncol = 6L)
  expect_names(names(data), identical.to = c(colnames, "..row_id"))

  # distinct
  expect_equal(backend$distinct(rows = NULL, cols = "y"), list(y = c(1, 0)))
  expect_equal(backend_classif$distinct(rows = NULL, cols = "y"), list(y = c("0", "1")))
  data = backend$distinct(rows = 1:100, cols = c("x_1", "y"))
  expect_names(names(data), identical.to = c("x_1", "y"))

  data_classif = backend_classif$distinct(rows = 1:100, cols = c("x_1", "y"))
  expect_character(c(data_classif$x_1, data_classif$y))

  # colnames
  expect_equal(backend$colnames, c("x_1", "x_2", "x_3", "x_4", "y", "..row_id"))

  # nrow
  expect_equal(backend$nrow, 49729)

  # ncol
  expect_equal(backend$ncol, 6)

  # stack
  expect_class(backend$stack, "RasterBrick")

  # data
  # [01] [02] [03] [04]
  # [05] [06] [07] [08]
  # [09] [10] [11] [12]
  raster = raster::brick(nrow = 3, ncol = 4)
  raster[] = 1:12
  names(raster) = "y"
  backend = DataBackendRasterBrick$new(raster, response = "y")

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

test_that("$missing works", {

  stack_classif = demo_stack_rasterbrick(size = 1, layers = 5)
  stack_classif_na = raster::setValues(stack_classif,
    c(NA, NA, runif(raster::ncell(stack_classif) * raster::nlayers(stack_classif) - 2)))
  backend = DataBackendRasterBrick$new(stack_classif_na, response = "y")

  expect_integer(backend$missings(rows = 1:10, "x_1"), names = "named", lower = 2, upper = 2)
  expect_integer(backend$missings(rows = 1:10, "x_2"), names = "named", lower = 0, upper = 0)

})
