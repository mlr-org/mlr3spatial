#' @title Predict on Spatial Objects with mlr3 Learners
#'
#' @description
#' This function allows to directly predict mlr3 learners on various spatial objects.
#'
#' @param task ([Task]).
#'   Task with [DataBackendRaster] or [DataBackendVector].
#' @param learner ([Learner]).
#'   Learner with trained model.
#' @template param-chunksize
#' @param format (`character(1)`)\cr
#'   Output class of the resulting object.
#'   Accepted values are `"raster"`, `"stars"` and `"terra"` if the input is a [DataBackendRaster].
#'   Note that when choosing something else than `"terra"`, the spatial object is converted into the respective format which might cause overhead both in runtime and memory allocation.
#'   For a [DataBackendVector], the output class will always be [sf::sf].
#' @param filename (`character(1)`)\cr
#'   Path where the spatial object should be written to.
#'
#' @return Spatial object of class given in argument `format`.
#' @examples
#' library(terra, exclude = "resample")
#'
#' # fit rpart on training points
#' task_train = tsk("leipzig")
#' learner = lrn("classif.rpart")
#' learner$train(task_train)
#'
#' # load raster and convert to task
#' stack = rast(system.file("extdata", "leipzig_raster.tif", package = "mlr3spatial"))
#' task_predict = as_task_unsupervised(stack, id = "leipzig")
#'
#' # predict land cover classes
#' pred = predict_spatial(task_predict, learner, chunksize = 1L)
#' @export
predict_spatial = function(task, learner, chunksize = 200L, format = "terra", filename = NULL) {
  assert_class(task$backend, "DataBackendRaster")
  assert_learner(learner)
  assert_task(task)
  assert_number(chunksize)
  stack = task$backend$stack
  start_time = proc.time()[3]
  learner = switch(learner$task_type,
    "classif" = LearnerClassifSpatial$new(learner),
    "regr" = LearnerRegrSpatial$new(learner))

  # calculate block size
  bs = block_size(stack, chunksize)

  # initialize target raster
  if (is.null(filename)) {
    filename = tempfile(fileext = ".tif")
  }
  target_raster = terra::rast(terra::ext(stack), resolution = terra::res(stack), crs = terra::crs(stack))
  terra::writeStart(target_raster, filename = filename, overwrite = TRUE, datatype = "FLT8S")

  lg$info("Start raster prediction")
  lg$info("Prediction is executed with a chunksize of %s Megabytes, %i chunk(s) in total, %i values per chunk",
    as.character(chunksize), length(bs$cells_seq), terra::ncell(task$backend$stack) / length(bs$cells_seq))

  mlr3misc::pmap(list(bs$cells_seq, bs$cells_to_read, seq_along(bs$cells_seq)), function(cells_seq, cells_to_read, n) {

    stack = task$backend$stack
    pred = learner$predict(task, row_ids = cells_seq:((cells_seq + cells_to_read - 1)))
    terra::writeValues(x = target_raster, v = pred$response,
      start = terra::rowFromCell(stack, cells_seq), # start row number
      nrows = terra::rowFromCell(stack, cells_to_read)) # how many rows
    lg$info("Chunk %i of %i finished", n, length(bs$cells_seq))
  })

  terra::writeStop(target_raster)
  lg$info("Finished raster prediction in %i seconds", as.integer(proc.time()[3] - start_time))

  if (learner$task_type == "classif") {
    levels = learner$learner$state$train_task$levels()[[learner$learner$state$train_task$target_names]]
    value = data.table(ID = seq_along(levels), categories = levels)
    target_raster = terra::categories(target_raster, value = value, index = 2)
  }
  target_raster = set_names(target_raster, learner$learner$state$train_task$target_names)

  switch(format,
    "terra" = target_raster,
    "stars" = stars::st_as_stars(target_raster),
    "raster" = as(target_raster, "Raster")
  )
}
