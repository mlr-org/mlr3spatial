test_that("as_task_classif_st works on data.frame objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))
  data = as.data.frame(vector)
  data$geometry = NULL
  data = cbind(data, st_coordinates(vector))

  task = as_task_classif_st(data, target = "y", coordinate_names = c("X", "Y"), crs = "EPSG:4326")
  expect_class(task, "TaskClassifST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, "EPSG:4326")
  expect_equal(task$col_roles$feature, "x_1")
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})

test_that("as_task_classif_st works on DataBackendDataTable objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))
  data = as.data.frame(vector)
  data$geometry = NULL
  data = cbind(data, st_coordinates(vector))
  backend = as_data_backend(data)

  task = as_task_classif_st(backend, target = "y", coordinate_names = c("X", "Y"), crs = "EPSG:4326")
  expect_class(task, "TaskClassifST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, "EPSG:4326")
  expect_equal(task$col_roles$feature, "x_1")
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})

test_that("as_task_classif_st works on sf objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))

  task = as_task_classif_st(vector, target = "y")
  expect_class(task, "TaskClassifST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, st_crs(vector)$wkt)
  expect_equal(task$col_roles$feature, "x_1")
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})

test_that("as_task_classif_st works on TaskClassifST objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))

  task = as_task_classif_st(vector, target = "y")
  task = as_task_classif_st(task)
  expect_class(task, "TaskClassifST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, st_crs(vector)$wkt)
  expect_equal(task$col_roles$feature, "x_1")
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})

test_that("convert from TaskRegrST to TaskClassifST", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("x_2", levels = c("a", "b")),
    numeric_layer("y")),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))

  task = as_task_regr_st(vector, target = "y")
  task = as_task_classif_st(task, target = "x_2", drop_original_target = TRUE)
  expect_class(task, "TaskClassifST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, st_crs(vector)$wkt)
  expect_equal(task$col_roles$feature, c("x_1", "y"))
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})
