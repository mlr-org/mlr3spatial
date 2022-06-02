# Predicts first feature in task
LearnerRegrFeature = R6Class("LearnerRegrFeature", inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "regr.featureless",
        feature_types = mlr_reflections$task_feature_types,
        predict_types = "response",
        param_set = paradox::ps(),
        properties = character(0),
        label = "Feature Regression Learner",
        man = "mlr3::mlr_learners_regr.featureless",
      )
    }
  ),

  private = list(
    .train = function(task) {
      set_class(list(feature = task$feature_names[1]), "regr.feature_model")
    },

    .predict = function(task) {
      list(response = task$data()[, self$model$feature, with = FALSE][[1]])
    }
  )
)
