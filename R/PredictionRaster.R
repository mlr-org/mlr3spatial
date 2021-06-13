#' @title Prediction Raster
#'
#' @description
#' This class handles raster predictions.
#'
#' @export
PredictionRaster = R6::R6Class("PredictionRaster",
  public = list(

    #' @field stack ([terra::SpatRaster]).
    stack = NULL,

    #' @field chunksize (`integer(1)`)\cr
    #' Size of raster chunks in megabyte.
    chunksize = NULL,

    #' @field reclassify_table ([data.table::data.table()])
    reclassify_table = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param stack ([terra::SpatRaster])\cr
    #' Raster stack of features.
    #' @param chunksize (`integer(1)`)\cr
    #' Size of raster chunks in megabyte.
    initialize = function(stack, chunksize = 100) {
      self$stack = assert_class(stack, "SpatRaster")
      self$chunksize = assert_int(chunksize, lower = 1)
    },

    #' @description
    #' Predicts raster with [mlr3::Learner].
    #'
    #' @param learner ([mlr3::Learner]).
    #' @param filename (`character`)\cr
    #' Output file path.
    predict = function(learner, filename = tempfile(fileext = ".grd")) {

      mlr3::assert_learner(learner)
      checkmate::assert_names(learner$state$train_task$feature_names, identical.to = names(self$stack))
      checkmate::assert_path_for_output(filename, overwrite = TRUE)

      stack = self$stack
      chunksize = self$chunksize
      start_time = Sys.time()

      # calculate block size
      tr = block_size(stack, chunksize)

      # initialize template raster
      template_raster = terra::rast(ext(stack), res = res(stack), crs = crs(stack))

      # open files for reading and writing
      terra::writeStart(template_raster, filename = filename, overwrite = TRUE)
      terra::readStart(stack)

      lg$info("Start raster prediction")
      lg$info("Prediction is executed in %i MB chunks", chunksize)

      for (i in 1:tr$n) {
        # read chunk of raster values
        new_data = as.data.table(terra::readValues(stack, row = tr$row[i], nrows = tr$nrows[i], dataframe = TRUE))

        # predict chunk
        pred = if ("parallel_predict" %in% learner$properties) {
          learner$predict_newdata_parallel(new_data)
        } else {
          learner$predict_newdata(new_data)
        }

        # reclassify predictions to integer values
        if (learner$state$train_task$task_type == "classif") {
          reclassify_table = data.table(task = task$class_names, raster = seq_along(task$class_names))
          pred = reclassify_table$raster[match(pred, reclassify_table$task)]
          self$reclassify_table = reclassify_table
        }

        terra::writeValues(template_raster, pred, tr$row[i], tr$nrows[i])

        lg$info("Chunk %i of %i finished", i, tr$n)
      }

      terra::writeStop(template_raster)
      terra::readStop(stack)
      lg$info("Finished raster prediction in %i seconds",
        as.integer(difftime(start_time, Sys.time(), units = "secs") * (-1))
      )
    }
  )
)
