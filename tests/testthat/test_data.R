test_that("categorical layer is set", {
  stack = generate_stack(list(
    factor_layer("y", levels = c("a", "b"))),
  dimension = 2)
  task = as_task_unsupervised(stack)
  expect_factor(task$data()$y, levels = c("a", "b"), len = 4)
})
