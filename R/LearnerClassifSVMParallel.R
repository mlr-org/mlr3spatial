#' Parallel Prediction SVM Classification Learner
#'
#' @description
#' Proof of concept for parallel prediction intergrated into the [mlr3::Learner]
#' class. The SVM classification learner is extended with the
#' `predict_newdata_parallel` method which splits `newdata` into `n` parts and
#' execudes the prediction in parallel. `n` is equal to the number of available
#' cores.
#'
#' @note
#' The `predict_newdata_parallel` method is not useful for learning algorithms
#' with an integrated parallel predict function. For example, the `ranger`
#' learner predicts newdata internally on all available cores. Therefore, a new
#' learner property (`parallel_predict`) should indicate if
#' `predict_newdata_parallel` should be called at all.
#'
#' @export
LearnerClassifSVMParallel = R6Class("LearnerClassifSVMParallel",
  inherit = LearnerClassifSVM,
  public = list(
    initialize = function() {
      super$initialize()
      self$properties = c(self$properties, "parallel_predict")
    },

    predict_newdata_parallel = function(newdata) {
      n = future::availableCores()
      nr = nrow(newdata)

      split_ids = rep(1:n, each=nr/n, length.out=nr)
      newdata = split(newdata, split_ids)

      res = future.apply::future_lapply(
        newdata,
        split_predict,
        self,
        future.globals = TRUE,
        future.scheduling = structure(TRUE, ordering = "random"),
        future.packages = c("mlr3", "mlr3spatial"))

      res = do.call("c", res)
      
      res$response
    }
  ))

split_predict = function(newdata, self) {
  self$predict_newdata(newdata)
}
