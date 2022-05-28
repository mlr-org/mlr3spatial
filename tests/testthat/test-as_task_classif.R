test_that("as_task_classif works on sf objects", {
  skip_if_not_installed("sf")
  requireNamespace("sf", quietly = TRUE)

  x = list(sf::st_point(c(1, 2)), sf::st_point(c(2, 1)))
  geometry = sf::st_sfc(x)
  vector = sf::st_sf(geometry)
  vector[, "y"] = factor(c("a", "b"))

  expect_class(as_task_classif(vector, target = "y"), "TaskClassif")
})

test_that("as_task_classif works on SpatRaster objects", {
  stack = demo_stack_spatraster(size = 1)
  terra::set.cats(stack, layer = 5, data.table(value = c(0, 1), y = c("a", "b")))
  expect_class(as_task_classif(stack, target = "y"), "TaskClassif")
})
