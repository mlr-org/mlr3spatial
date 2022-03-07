test_that("DataBackendRaster works", {
  backend = DataBackendRaster$new(generate_spat_raster())

  # head
  data = backend$head(10L)
  expect_data_table(data, nrow = 10L, ncol = 5L)
  expect_names(names(data), identical.to = c("x_1", "x_2", "x_3", "x_4", "y"))

  # distinct
  expect_equal(backend$distinct(rows = NULL, cols = "y"), list(y = c("negative", "positive")))
  data = backend$distinct(rows = 1:100, cols = c("x_1", "y"))
  expect_names(names(data), identical.to = c("x_1", "y"))
  expect_numeric(data$x_1)
  expect_factor(data$y, levels = c("negative", "positive"))
  # non-factor, all rows
  data = backend$distinct(rows = NULL, cols = "x_1")
  expect_names(names(data), identical.to = "x_1")
  expect_length(data[[names(data)]], 4900)

  # nrow
  expect_equal(backend$nrow, 4900)

  # ncol
  expect_equal(backend$ncol, 6)

  stack_classif = terra::rast(nrow = 3, ncol = 4)
  stack_classif[] = c(1:11, NA)
  names(stack_classif) = "y"
  # backend = as_data_backend(stack_classif)
  expect_equal(backend$distinct(rows = 1:12, cols = "y"),
    list(y = factor("negative", levels = c("negative", "positive"))))

  # terra does not add NA as a factor level
  stack_classif = terra::rast(nrow = 2, ncol = 2)
  stack_classif[] = c(0, 1, NA, 0)
  names(stack_classif) = "y"
  value = data.table(ID = c(0, 1), y = c("negative", "positive"))
  terra::set.cats(stack_classif, layer = "y", value = value)
  # backend = as_data_backend(stack_classif)
  expect_equal(levels(backend$distinct(rows = 1:4, cols = "y")[[1]]), c("negative", "positive"))

  # missings
  stack_classif = terra::rast(nrow = 2, ncol = 2)
  stack_classif[] = c(0, 1, NA, 0)
  names(stack_classif) = "y"
  # backend = as_data_backend(stack_classif)
  backend$missings(rows = 1:4, cols = "y")

  # data prototyp
  expect_data_table(backend$data(rows = integer(0), cols = c("x_1", "y")), nrows = 0, ncols = 2)

  # data
  # [01] [02] [03] [04]
  # [05] [06] [07] [08]
  # [09] [10] [11] [12]
  stack_classif = terra::rast(nrow = 3, ncol = 4)
  stack_classif[] = 1:12
  names(stack_classif) = "y"
  backend = DataBackendRaster$new(stack_classif)

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

# stars input ------------------------------------------------------------------

test_that("DataBackendRaster + stars", {
  backend = as_data_backend(generate_stars())

  # head
  data = backend$head(10L)
  expect_data_table(data, nrow = 10L, ncol = 6L)
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
  expect_data_table(data, nrow = 10L, ncol = 5L)
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
  expect_data_table(data, nrow = 10L, ncol = 1L)
  expect_names(names(data), identical.to = "y")

  # distinct
  # no support for factors when using bricks
  expect_equal(backend$distinct(rows = 1:1000, cols = "y"),
    list("y" = c(1, 0)))
  data = backend$distinct(rows = 1:5, cols = "y")
  expect_names(names(data), identical.to = "y")
  expect_numeric(data$y)

})
