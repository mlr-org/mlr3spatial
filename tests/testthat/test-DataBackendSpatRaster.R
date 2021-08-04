test_that("DataBackendSpatRaster works", {
  # prepare raster stack
  stack = rast(spatraster)
  value = data.table(ID = c(0, 1), y = c("negative", "positive"))
  terra::setCats(stack, layer = "y", value = value)
  colnames = names(stack)

  backend = DataBackendSpatRaster$new(stack)

  # head
  data = backend$head(10L)
  expect_data_table(data, nrow = 10L, ncol = 5L)
  expect_names(names(data), identical.to = colnames)

  # distinct
  expect_equal(backend$distinct(rows = NULL, cols = "y"), list(y = c("negative", "positive")))
  data = backend$distinct(rows = 1:100, cols = c("x_1", "y"))
  expect_names(names(data), identical.to = c("x_1", "y"))
  expect_numeric(data$x_1)
  expect_factor(data$y, levels = c("negative", "positive"))

  # data
  # [01] [02] [03] [04]
  # [05] [06] [07] [08]
  # [09] [10] [11] [12]
  spatraster = rast(nrow = 3, ncol = 4)
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


# FIXME: test as_task_classif
# FIXME: test printer
