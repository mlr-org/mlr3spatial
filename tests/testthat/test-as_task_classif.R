test_that("as_task_classif works on sf objects", {
  skip_if_not_installed("sf")
  requireNamespace("sf", quietly = TRUE)

  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10)
  vector = sf::st_as_sf(sample_stack(stack))

  expect_class(as_task_classif(vector, target = "y"), "TaskClassif")
})

test_that("as_task_classif works on SpatRaster objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10)

  expect_class(as_task_classif(stack, target = "y"), "TaskClassif")
})
