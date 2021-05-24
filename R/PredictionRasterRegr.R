#' Demo title
#' @description demo descrip
#' @export
PredictionRasterRegr = R6::R6Class("PredictionRasterRegr",
  inherit = PredictionRaster,
  public = list(
    initialize = function(stack, task, reclassify_table) {
      super$initialize(stack = stack, task = task)
    }
  ),
  private = list(
    .classify = function(pred) {
      pred
    }
  )
)
