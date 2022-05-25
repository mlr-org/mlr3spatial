PipeOpLearnerNA = R6Class("PipeOpLearnerNA", inherit = PipeOpLearner,
  private = list(
    .predict = function(inputs) {
      on.exit({private$.learner$state = NULL})
      task = inputs[[1]]
      private$.learner$state = self$state

      data = task$data()
      ids = complete.cases(data[, task$feature_names, with = FALSE])
      pred = private$.learner$predict_newdata(data[ids])
      response = rep(NaN, nrow(data))
      response[ids] = pred$data$response
      pred$data$row_ids = seq_len(nrow(data))
      pred$data$response = response
      pred$data$truth = rep(NaN, nrow(data))
      browser()
      list(pred)
    }
  )
)
