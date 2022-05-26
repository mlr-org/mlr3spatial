test_that("LearnerRegrSpatial ignores observations with missing values", {
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  # train task
  stack = create_stack(list(
    factor_layer("c_1", levels = c("a", "b")),
    numeric_layer("y")),
  dimension = 10)
  vector = create_vector(stack, n = 10)
  task_train = as_task_regr(vector, id = "test_vector", target = "y")
  learner = lrn("regr.ranger")
  learner$train(task_train)

  # predict task
  stack$y = NULL
  stack = add_aoi(stack)
  backend = DataBackendRaster$new(stack, task_train)
  task_predict = as_task_regr(backend, id = "test", target = "y")
  learner_spatial = LearnerRegrSpatial$new(learner)
  pred = learner_spatial$predict(task_predict)

  expect_true(all(is.na(pred$response[seq(10)])))
  expect_numeric(pred$response[11:100], any.missing = FALSE, all.missing = FALSE)
})
