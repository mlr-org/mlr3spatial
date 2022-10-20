test_that("TaskRegrST throws an error when backend is an sf object", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))

  expect_error(TaskRegrST$new(id = "test", backend = vector, target = "y"), "convert an sf objects into a task")
})
