#' @title Predict on spatial objects with mlr3 learners
#' @description This function allows to directly predict mlr3 learners on
#' various spatial objects (see section "Supported Spatial Classes"). It returns
#' an [mlr3::Prediction] object and (optionally) the same object that was used
#' for the prediction.
#' @param task ([Task]).
#' @param learner ([Learner]).
#' @template param-chunksize
#' @param format `[character]`\cr
#' Output class of the resulting object. Accepted values are `"raster"`,
#' `"stars"` and `"terra"` if the input is a [DataBackendRaster]. Note that when
#' choosing something else than `"terra"`, the spatial object is converted into
#' the respective format which might cause overhead both in runtime and memory
#' allocation.
#' For a [DataBackendVector], the output class will always be [sf::sf].
#' @param filename `[character]`\cr
#' Path where the spatial object should be written to.
#' @details
#' When parallelizing the prediction via {future}, plan `"multisession"` will
#' not work due to external pointers within the spatial object. If the execution
#' platform is UNIX-based, `plan("multicore")` is recommended. For Windows
#' users, `plan(future.callr::callr)` might be an alternative.
#'
#' @return Spatial object of class given in argument `format`.
#' @examples
#' stack = demo_stack_spatraster(size = 1)
#' value = data.table::data.table(ID = c(0, 1), y = c("negative", "positive"))
#' terra::set.cats(stack, layer = "y", value = value)
#'
#' # create backend
#' backend = as_data_backend(stack)
#' task = as_task_classif(backend, target = "y", positive = "positive")
#' # train
#' learner = lrn("classif.featureless")
#' learner$train(task, row_ids = sample(1:task$nrow, 50))
#' ras = predict_spatial(task, learner)
#' ras
#' @export
predict_spatial = function(task, learner, chunksize = 200L, format = "terra",
  filename = NULL) {
  assert_multi_class(task$backend, c("DataBackendRaster", "DataBackendVector"))
  assert_learner(learner)
  assert_task(task)
  assert_int(chunksize)
  stack = task$backend$stack
  start_time = proc.time()[3]


  if (inherits(task$backend, "DataBackendRaster")) {

    # calculate block size
    bs = block_size(stack, chunksize)

    # initialize target raster
    if (is.null(filename)) {
      filename = tempfile(fileext = ".tif")
    }
    target_raster = terra::rast(terra::ext(stack), resolution = terra::res(stack), crs = terra::crs(stack))
    terra::writeStart(target_raster, filename = filename, overwrite = TRUE, datatype = "FLT8S")

    lg$info("Start raster prediction")
    lg$info("Prediction is executed with a chunksize of %i, %i chunk(s) in total, %i values per chunk",
      chunksize, length(bs$cells_seq), terra::ncell(task$backend$stack) / length(bs$cells_seq))

    mlr3misc::pmap(list(bs$cells_seq, bs$cells_to_read, seq_along(bs$cells_seq)), function(cells_seq, cells_to_read, n) {

      stack = task$backend$stack
      pred = learner$predict(task, row_ids = cells_seq:((cells_seq + cells_to_read - 1)))
      terra::writeValues(target_raster, pred$response,
        terra::rowFromCell(stack, cells_seq),
        terra::rowFromCell(stack, cells_to_read))
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
  } else {
    pred = learner$predict(task)

    sf_pred = sf::st_as_sf(data.frame(pred = pred$response, geometry = task$backend$geometry))
  }
  if (!is.null(filename)) {
    sf::st_write(sf_pred, filename, quiet = TRUE)
  }
  return(sf_pred)
}
