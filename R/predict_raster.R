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
  lg$info("Prediction is executed in %i MB chunks, %i chunk(s) in total, %i values per chunk",
    chunksize, length(bs$cells_seq), terra::ncell(task$backend$stack))

  pmap(list(bs$cells_seq, bs$cells_to_read, seq_along(bs$cells_seq)), function(cells_seq, cells_to_read, n) {

    pred = learner$predict(task, row_ids = cells_seq:((cells_seq + cells_to_read - 1)))
    terra::writeValues(target_raster, pred$response,
      terra::rowFromCell(task$backend$stack, cells_seq),
      terra::rowFromCell(task$backend$stack, cells_to_read))
    lg$info("Chunk %i of %i finished", n, length(bs$cells_seq))
  })

  terra::writeStop(target_raster)
  lg$info("Finished raster prediction in %i seconds", as.integer(proc.time()[3] - start_time))
  target_raster
}
