test_that("AutoTuner works with a spatial backends", {

  with_future("multisession", workers = 2,

    for (backend in c(task_spatraster, task_rasterbrick, task_stars)) {
      at = AutoTuner$new(
        learner = learner,
        resampling = rsmp("cv", folds = 3),
        measure = msr("classif.ce"),
        search_space = tune_ps,
        terminator = terminator,
        tuner = tuner
      )
      grid = benchmark_grid(
        task = backend,
        learner = list(at),
        resampling = rsmp("cv", folds = 3)
      )

      bmr = benchmark(grid)

      expect_class(bmr, "BenchmarkResult")
      expect_equal(bmr$resamplings$resampling_id, "cv")
      expect_data_table(bmr$score(), nrows = 3)
    }
  )
})
