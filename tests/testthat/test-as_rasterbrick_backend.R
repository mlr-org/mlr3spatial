test_that("as_spatraster_backend from spatraster", {
  stack_classif = demo_stack_rasterbrick(size = 1, layers = 5)
  colnames = names(stack_classif)
  backend = as_rasterbrick_backend(stack_classif, response = "y", response_is_factor = TRUE)

  # head
  data = backend$head(10L)
  expect_data_table(data, nrow = 10L, ncol = 6L)
  expect_names(names(data), identical.to = c(colnames, "..row_id"))

  # distinct
  expect_equal(backend$distinct(rows = NULL, cols = "y"), list(y = c("0", "1")))
  data = backend$distinct(rows = 1:100, cols = c("x_1", "y"))
  expect_names(names(data), identical.to = c("x_1", "y"))

  data_classif = backend$distinct(rows = 1:100, cols = c("x_1", "y"))
  expect_character(c(data_classif$x_1, data_classif$y))

  # colnames
  expect_equal(backend$colnames, c("x_1", "x_2", "x_3", "x_4", "y", "..row_id"))

  # nrow
  expect_equal(backend$nrow, 49729)

  # ncol
  expect_equal(backend$ncol, 6)

  # stack
  expect_class(backend$stack, "RasterBrick")
})
