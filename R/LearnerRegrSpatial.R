LearnerRegrSpatial = R6::R6Class("LearnerRegrSpatial",
  inherit = LearnerRegr,
  public = list(
    learner = NULL,

    initialize = function(learner) {
      self$learner = assert_learner(learner)
      super$initialize(
        id = "regr.ranger",
        param_set = learner$param_set,
        predict_types = learner$predict_types,
        feature_types = learner$feature_types,
        properties = union(learner$properties, "missings"),
        packages = learner$packages,
        man = "mlr3learners::mlr_learners_regr.spatial"
      )
    },

    predict = function(task, row_ids = NULL) {
      data = task$data(rows = row_ids)
      ids = complete.cases(data[, task$feature_names, with = FALSE])
      pred = self$learner$predict_newdata(data[ids])
      response = rep(NaN, nrow(data))
      response[ids] = pred$data$response
      pred$data$row_ids = seq_len(nrow(data))
      pred$data$response = response
      pred$data$truth = rep(NaN, nrow(data))
      pred
    }
  )
)
