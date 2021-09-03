#' @export
predict_raster = function(task, learner, chunksize = 100L, filename = tempfile(fileext = ".tif")) {
  assert_learner(learner)
  assert_task(task)
  assert_int(chunksize)
  assert_path_for_output(filename)
  stack = task$backend$stack
  start_time = proc.time()[3]

  # calculate block size
  bs = block_size(stack, chunksize)

  # initialize target raster
  target_raster = terra::rast(terra::ext(stack), res = terra::res(stack), crs = terra::crs(stack))
  terra::writeStart(target_raster, filename = filename)

  lg$info("Start raster prediction")
  lg$info("Prediction is executed in %i MB chunks", chunksize)

  pmap(list(bs$row, bs$nrows, seq_along(bs$row)), function(row, nrows, n) {
    pred = learner$predict(task, row_ids = row:(row + nrows - 1))
    terra::writeValues(target_raster, pred$response, row, nrows)
    lg$info("Chunk %i of %i finished", n, length(bs$row))
  })

  terra::writeStop(target_raster)
  lg$info("Finished raster prediction in %i seconds", as.integer(proc.time()[3] - start_time))
  target_raster
}
