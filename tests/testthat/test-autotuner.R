test_that("AutoTuner works with a spatial backends", {

  library("mlr3tuning")
  library("paradox")

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")

  logger_mlr3 = lgr::get_logger("mlr3")
  logger_mlr3$set_threshold("warn")

  learner = lrn("classif.rpart")
  tune_ps = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))
  terminator = trm("evals", n_evals = 2)
  tuner = tnr("random_search")
  backend_sp = as_data_backend(stack_classif)
  task = as_task_classif(backend_sp, target = "y", positive = "positive")

  with_future("multisession", workers = 2, {

    at = AutoTuner$new(
      learner = learner,
      resampling = rsmp("cv", folds = 3),
      measure = msr("classif.ce"),
      search_space = tune_ps,
      terminator = terminator,
      tuner = tuner
    )
    grid = benchmark_grid(
      task = task,
      learner = list(at),
      resampling = rsmp("cv", folds = 3)
    )

    bmr = benchmark(grid)

    expect_class(bmr, "BenchmarkResult")
    expect_equal(bmr$resamplings$resampling_id, "cv")
    expect_data_table(bmr$score(), nrows = 3)
  })
})
