test_that("DataBackendSpatial works", {
  backend = DataBackendSpatial$new(stack_classif)

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
  terra::setCats(stack_classif, layer = "y", value = value)
  # backend = as_data_backend(stack_classif)
  expect_equal(levels(backend$distinct(rows = 1:4, cols = "y")[[1]]), c("negative", "positive"))

  # missings
  stack_classif = terra::rast(nrow = 2, ncol = 2)
  stack_classif[] = c(0, 1, NA, 0)
  names(stack_classif) = "y"
  # backend = as_data_backend(stack_classif)
  backend$missings(rows = 1:4, cols = "y")

  # data
  # [01] [02] [03] [04]
  # [05] [06] [07] [08]
  # [09] [10] [11] [12]
  stack_classif = terra::rast(nrow = 3, ncol = 4)
  stack_classif[] = 1:12
  names(stack_classif) = "y"
  backend = DataBackendSpatial$new(stack_classif)

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

# stars input ------------------------------------------------------------------

test_that("DataBackendSpatial works", {
  backend = DataBackendSpatial$new(l7data)

  # head
  data = backend$head(10L)
  expect_data_table(data, nrow = 10L, ncol = 6L)
  expect_names(names(data), identical.to = colnames_stars)

  # distinct
  expect_equal(backend$distinct(rows = 1:10, cols = "layer.1"),
    list("layer.1" = c(69, 63, 60, 61, 62, 64)))
  data = backend$distinct(rows = 1:5, cols = c("layer.1", "layer.2"))
  expect_names(names(data), identical.to = c("layer.1", "layer.2"))
  expect_numeric(data$layer.1)

})

# brick input ------------------------------------------------------------------

test_that("DataBackendSpatial works", {
  backend = DataBackendSpatial$new(stack_brick)

  # head
  data = backend$head(10L)
  expect_data_table(data, nrow = 10L, ncol = 5L)
  expect_names(names(data), identical.to = colnames)

  # distinct
  # no support for factors when using bricks
  expect_equal(backend$distinct(rows = 1:1000, cols = "y"),
    list("y" = c(1, 0)))
  data = backend$distinct(rows = 1:5, cols = c("y", "x_2"))
  expect_names(names(data), identical.to = c("y", "x_2"))
  expect_numeric(data$y)

})
