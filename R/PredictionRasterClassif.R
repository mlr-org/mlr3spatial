#' @export
PredictionRasterClassif = R6::R6Class("PredictionRasterClassif",
  inherit = PredictionRaster,
  public = list(
    reclassify_table = NULL,

    initialize = function(stack, task, reclassify_table = NULL) {
      super$initialize(stack = stack, task = task)

      self$reclassify_table = if(is.null(reclassify_table)) {
        data.table(task = task$class_names,
                   raster = seq(length(task$class_names)))
      } else {
        reclassify_table
      }
    }
  ),

  private = list(
    .classify = function(pred) {
      rt = self$reclassify_table
      rt$raster[match(pred, rt$task)]
    }
  )
)
