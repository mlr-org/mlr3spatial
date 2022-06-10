test_that("LearnerClassifSpatial ignores observations with missing values", {
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  # train task
  stack = generate_stack(list(
    numeric_layer("x_1"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 10)
  vector = sample_stack(stack, n = 10)
  task_train = as_task_classif(vector, id = "test_vector", target = "y")
  learner = lrn("classif.ranger")
  learner$train(task_train)

  # predict task
  stack$y = NULL
  stack = mask_stack(stack)
  task_predict = as_task_unsupervised(stack, id = "test")
  learner_spatial = LearnerClassifSpatial$new(learner)
  pred = learner_spatial$predict(task_predict)

  expect_true(all(is.na(pred$response[seq(10)])))
  expect_numeric(pred$response, any.missing = TRUE, all.missing = FALSE)
})
