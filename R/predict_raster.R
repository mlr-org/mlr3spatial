#' @export
predict_raster = function(task, learner, chunksize = 100L, filename) {
  assert_learner(learner)
  assert_task(task)
  assert_int(chunksize)
  assert_path_for_output(filename)
  stack = task$backend$stack
  start_time = proc.time()[3]

  # calculate block size
  bs = block_size(stack, chunksize)

  # initialize target raster
  target_raster = terra::rast(ext(stack), res = res(stack), crs = crs(stack))
  writeStart(target_raster, filename = filename)

  lg$info("Start raster prediction")
  lg$info("Prediction is executed in %i MB chunks", chunksize)

  for (i in seq_len(bs$n)) {
    pred = learner$predict(task, row_ids = bs$row[i]:(bs$row[i] + bs$nrows[i] - 1))
    terra::writeValues(target_raster, pred$response, bs$row[i], bs$nrows[i])
    lg$info("Chunk %i of %i finished", i, bs$n)
  }

  writeStop(target_raster)
  lg$info("Finished raster prediction in %i seconds", proc.time()[3] - start_time)
}
