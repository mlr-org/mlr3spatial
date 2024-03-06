#' @title Predict on Spatial Objects with mlr3 Learners
#'
#' @description
#' This function allows to directly predict mlr3 learners on various spatial objects.
#'
#' @param newdata ([terra::SpatRaster] | `stars::stars` | [sf::sf] | `raster::RasterStack` | `raster::RasterBrick`).
#'   New data to predict on. All spatial data formats convertible by `as_data_backend()` are supported e.g. [terra::SpatRaster] or [sf::sf].
#' @param learner ([Learner]).
#'   Learner with trained model.
#' @template param-chunksize
#' @param format (`character(1)`)\cr
#'   Output class of the resulting object.
#'   Accepted values are `"raster"`, `"stars"` and `"terra"` if the input is a raster.
#'   Note that when choosing something else than `"terra"`, the spatial object is converted into the respective format which might cause overhead both in runtime and memory allocation.
#'   For vector data only `"sf"` is supported.
#' @param filename (`character(1)`)\cr
#'   Path where the spatial object should be written to.
#' @param predict_type (`character(1)`)\cr
#'  Type of prediction to return.
#'  Accepted values are `"response"` and `"prob"`.
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
#' # load raster
#' stack = rast(system.file("extdata", "leipzig_raster.tif", package = "mlr3spatial"))
#'
#' # predict land cover classes
#' pred = predict_spatial(stack, learner, chunksize = 1L)
#' @export
predict_spatial = function(newdata, learner, chunksize = 200L, format = "terra", filename = NULL, predict_type = "response") {
  task = as_task_unsupervised(newdata)
  assert_multi_class(task$backend, c("DataBackendRaster", "DataBackendVector"))
  assert_learner(learner)
  assert_choice(predict_type, c("response", "prob"))

  if (test_class(task$backend, "DataBackendRaster")) {
    assert_number(chunksize)
    assert_choice(format, c("terra", "raster", "stars"))
    filename = filename %??% tempfile(fileext = ".tif")
    assert_path_for_output(filename)

    stack = task$backend$stack
    start_time = proc.time()[3]
    learner = switch(learner$task_type,
      "classif" = LearnerClassifSpatial$new(learner),
      "regr" = LearnerRegrSpatial$new(learner))

    # calculate block size
    bs = block_size(stack, chunksize)

    # initialize target raster
    target_raster = terra::rast(terra::ext(stack), resolution = terra::res(stack), crs = terra::crs(stack))
    terra::writeStart(target_raster, filename = filename, overwrite = TRUE, datatype = "FLT8S")

    lg$info("Start raster prediction")
    lg$info("Prediction is executed with a chunksize of %s Megabytes, %i chunk(s) in total, %i values per chunk",
      as.character(chunksize), length(bs$cells_seq), ceiling(terra::ncell(task$backend$stack) / length(bs$cells_seq)))

    mlr3misc::pmap(list(bs$cells_seq, bs$cells_to_read, seq_along(bs$cells_seq)), function(cells_seq, cells_to_read, n) {

      stack = task$backend$stack
      pred = learner$predict(task, row_ids = cells_seq:((cells_seq + cells_to_read - 1)))
      vals = if (predict_type == "prob") pred$prob[, learner$learner$state$train_task$positive] else pred$response
      terra::writeValues(x = target_raster, v = vals,
        start = terra::rowFromCell(stack, cells_seq), # start row number
        nrows = terra::rowFromCell(stack, cells_to_read)) # how many rows
      lg$info("Chunk %i of %i finished", n, length(bs$cells_seq))
    })

    terra::writeStop(target_raster)
    lg$info("Finished raster prediction in %i seconds", as.integer(proc.time()[3] - start_time))

    if (learner$task_type == "classif" && predict_type == "response") {
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
  } else {
    assert_string(format, "sf")
    if (!is.null(filename)) assert_path_for_output(filename)
    pred = learner$predict(task)
    vector = set_names(sf::st_as_sf(data.frame(pred$response, task$backend$sfc)), c(learner$state$train_task$target_names, "geometry"))

    if (!is.null(filename)) sf::st_write(vector, filename, quiet = TRUE)
    vector
  }
}
