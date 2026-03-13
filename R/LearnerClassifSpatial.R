LearnerClassifSpatial = R6::R6Class("LearnerClassifSpatial",
  inherit = LearnerClassif,
  public = list(
    learner = NULL,

    initialize = function(learner) {
      self$learner = assert_learner(learner)
      super$initialize(
        id = learner$id,
        param_set = learner$param_set,
        predict_types = learner$predict_types,
        feature_types = learner$feature_types,
        properties = union(learner$properties, "missings"),
        packages = learner$packages,
        man = "mlr3learners::mlr_learners_classif.spatial"
      )
      self$predict_type = learner$predict_type
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
      if (self$learner$predict_type == "prob") {
        prob = matrix(NaN, nrow = nrow(data), ncol = 2)
        prob[ids, 1] = pred$data$prob[, 1]
        prob[ids, 2] = pred$data$prob[, 2]
        attributes(prob) = attributes(pred$prob)
        pred$data$prob = prob
      }
      pred
    }
  )
)
