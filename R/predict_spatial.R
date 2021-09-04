#' @title Predict on spatial objects with mlr3 learners
#' @description
#' This function allows to directly predict mlr3 learners on various spatial
#' objects (see section "Supported Spatial Classes"). It returns an
#' [mlr3::Prediction] object and (optionally) the same object that was used for
#' the prediction.
#' @param object `[SpatRaster, sf, RasterBrick]`
#' @param learner [mlr3::Learner]\cr
#'   Any [mlr3::Learner].
#' @param filename `[character]`\cr
#'   Filename of optional file to write prediction values into.
#'   For raster-like inputs this can be a `.tif` file.
#'   For {sf} objects, this could be a `.gpgk` or a `.shp` file.
#' @param overwrite `[logical]`\cr
#'   Should a possibly existing file on disk (referring to argument `filename`)
#'   be overwritten?
#' @param quiet `[logical]`\cr
#'   Whether to suppress possible console output when invoking the writing
#'   methods of the respective spatial classes.
#' @details
#' A direct prediction on a subset of a [mlr3::Task] object is not possible for
#' \CRANpkg{terra} objects as \CRANpkg{terra} objects contain external pointers
#' which are not compatible with future-based parallelization. Due to this, the
#' values from the \CRANpkg{terra} object need to be extracted first into a
#' `data.table`.
#'
#' @section Parallelization:
#'
#' For predictions which take > 10 seconds, parallelization could help speeding
#' things up. {mlr3} supports parallel predictions since v0.12.0. This can be
#' enabled by setting the `$parallel_predict = TRUE` flag in the learner and
#' supplying a parallel future plan before executing the prediction, for example
#' `future::plan(multisession, workers = 2)`. See the examples for more
#' information.
#'
#' @section Spatial Classes support:
#'
#' Task and Prediction support for the following classes is planned:
#'
#' - {sf}
#' - {stars}
#' - {raster}
#'
#' @return mlr3::Prediction
#' @examples
#' if (mlr3misc::require_namespaces(c("terra", "future"), quietly = TRUE)) {
#'   stack = demo_stack_spatraster(size = 5, layers = 5)
#'   backend = DataBackendSpatRaster$new(stack)
#'   task = as_task_classif(backend, target = "y", positive = "TRUE")
#'   # train
#'   learner = lrn("classif.featureless")
#'   learner$train(task, row_ids = sample(1:task$nrow, 500))
#'   predict_spatial_newdata(learner, stack)
#' }
#' @export
predict_spatial = function(task, learner, chunksize = 100L, format = "terra") {
  assert_class(task$backend, "DataBackendSpatial")
  assert_learner(learner)
  assert_task(task)
  assert_int(chunksize)
  stack = task$backend$stack
  start_time = proc.time()[3]

  # calculate block size
  bs = block_size(stack, chunksize)

  # initialize target raster
  target_raster = terra::rast(terra::ext(stack), res = terra::res(stack), crs = terra::crs(stack))
  terra::writeStart(target_raster, filename = tempfile(fileext = ".tif"), overwrite = TRUE)

  lg$info("Start raster prediction")
  lg$info("Prediction is executed with a chunksize of %i, %i chunk(s) in total, %i values per chunk",
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

  target_raster = switch(format,
    "terra" = target_raster,
    "stars" = stars::st_as_stars(target_raster),
    "raster" = as(target_raster, "Raster")
  )

  return(target_raster)
}
