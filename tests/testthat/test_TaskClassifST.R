test_that("TaskClassifST throws an error when backend is an sf object", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10)
  vector = st_as_sf(sample_stack(stack, n = 100))

  expect_error(TaskClassifST$new(id = "test", backend = vector, target = "y"), "convert an sf objects into a task")
})
