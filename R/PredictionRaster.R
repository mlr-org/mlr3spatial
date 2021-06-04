#' @export
PredictionRaster = R6::R6Class("PredictionRaster",
  public = list(
    stack = NULL,
    task = NULL,
    chunksize = 100,

    initialize = function(stack, task) {
      self$stack = stack
      self$task = task
    },

    predict = function(learner, filename = tempfile(fileext = ".grd")) {
      stack = self$stack
      chunksize = self$chunksize
      start_time = Sys.time()

      # Calculate block size
      tr = raster::blockSize(stack, chunksize = chunksize*1e+06, n = 4, minblocks = 1)

      # Initialize target raster
      template_raster = terra::rast(ext(stack), res = res(stack), crs = crs(stack))
      
      # `target_raster` could replace `raster::blockSize`
      target_raster = terra::writeStart(template_raster, filename=filename,  overwrite=TRUE)

      terra::readStart(stack)

      lg$info("Start raster prediction")
      lg$info("Prediction is executed in %i MB chunks", chunksize)

      for(i in 1:tr$n) {

        new_data = as.data.table(terra::readValues(stack, row=tr$row[i], nrows=tr$nrows[i], dataframe = TRUE))

        if("parallel_predict" %in% learner$properties) {
          pred = learner$predict_newdata_parallel(new_data)
        } else {
          pred = learner$predict_newdata(new_data)
        }

        pred = private$.classify(pred)

        terra::writeValues(template_raster, pred, tr$row[i], tr$nrows[i])

        lg$info("Chunk %i of %i finished", i, tr$n)
      }
      
      lg$info("Finished raster prediction in %i seconds", as.integer(difftime(start_time, Sys.time(), units = "secs")*(-1)))

      terra::readStop(stack)
      terra::writeStop(template_raster)
    }
  ),
  private = list(
    .classify = function() {
      stop("Abstract class")
    }
  ),
  active = list(

    #' @field raster_size
    #' Returns raster size in memory
    raster_size = function() {
      c("Megabyte" = ncell(self$stack)*nlayers(self$stack)*8/1e+06)
    }
  )
)

