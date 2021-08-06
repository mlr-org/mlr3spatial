#' @importFrom terra rast ext res crs writeStart writeValues writeStop
#' @export
predict_spatial = function(task, learner, chunksize = 100L, filename = NULL, overwrite = TRUE) {
  assert_learner(learner)
  assert_task(task)
  assert_int(chunksize)
  stack = task$backend$stack
  start_time = proc.time()[3]

  if (is.null(filename)) {
    filename = tempfile(fileext = ".tif")
  }
  assert_path_for_output(filename, overwrite = overwrite)

  # browser()

  # calculate block size
  # FIXME: obsolet when using mlr3 parallel predict?
  # bs = block_size(stack, chunksize)

  # initialize target raster with only NA values using the same metadata as stored in the backend
  # arg `vals`: we need to set some dummy values, otherwise setValues() further down does not work
  # FIXME: do we need to init with nlyrs?!
  target_raster = terra::rast(terra::ext(stack), res = terra::res(stack),
    # nlyrs = terra::nlyr(stack),
    crs = terra::crs(stack), vals = NA)
  # terra::writeStart(target_raster, filename = filename, overwrite = overwrite)
  # browser()

  #### Marcs approach start ------
  # lg$info("Start raster prediction")
  # lg$info("Prediction is executed in %i MB chunks", chunksize)

  # pred = learner$predict(task)
  # terra::writeValues(target_raster, pred$response, row, nrows)

  # FIXME: the final raster has like 12 mio values, we only predict 14k? how does the final raster still have 12 mio values?
  # FIXME: if we do it in chunks we might get in trouble with the native mlr3 parallelism which also splits the prediction into chunks but does not account for memory?
  # FIXME: shouldn't we sequence along task$nrow?
  # mlr3misc::pmap(list(bs$row, bs$nrows, seq_along(task$nrow)), function(row, nrows, n) {
  #   # Our task has 12496225 rows?
  #   # we should have 708 (nrow) * 3535 (ncol) * 5 (nlyr) values here, no? instead the response is of length 708 only
  #   pred = learner$predict(task, row_ids = row:(row + nrows - 1))
  #   browser()
  #   terra::writeValues(target_raster, pred$response, row, nrows)
  #   lg$info("Chunk %i of %i finished", n, length(bs$row))
  # })

  #### Marcs approach end ------

  #### pat-s approach start ------

  # uses mlr3's new parallel predict when activated
  # we always want to predict all values, otherwise setValues() won't work, subset here is ONLY FOR TESTING
  # pred = learner$predict(task, row_ids = 1:100000)
  # browser()
  pred = learner$predict(task)

  # browser()

  terra::setValues(target_raster, pred$response)
  terra::writeRaster(target_raster, filename, overwrite = overwrite)
  return(invisible(pred))

  #### pat-s approach end ------

  # terra::writeStop(target_raster)
  # lg$info("Finished raster prediction in %i seconds", as.integer(proc.time()[3] - start_time))

}

predict_spatial_newdata = function(newdata, learner, filename = NULL, overwrite = TRUE) {

  if (is.null(filename)) {
    filename = tempfile(fileext = ".tif")
  }
  assert_path_for_output(filename, overwrite = overwrite)

  # read cell values from raster stack
  # this is done implicitly also when predicting on a task internally, i.e. task$data() is called
  terra::readStart(stack)
  on.exit(terra::readStop(stack))
  newdata = as.data.table(terra::readValues(stack, dataframe = TRUE))

  # browser()

  pred = learner$predict_newdata(newdata)

}
