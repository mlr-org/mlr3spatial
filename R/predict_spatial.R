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
#'
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
#'   stack = demo_stack(size = 5, layers = 5)
#'   backend = DataBackendSpatRaster$new(stack)
#'   task = as_task_classif(backend, target = "y", positive = "TRUE")
#'   # train
#'   learner = lrn("classif.featureless")
#'   learner$train(task, row_ids = sample(1:task$nrow, 500))
#'   predict_spatial_newdata(learner, stack)
#' }
#' @export
predict_spatial_newdata = function(learner, object, filename = NULL, overwrite = FALSE) {
  UseMethod("predict_spatial_newdata", object = object)
}

#' @export
predict_spatial_newdata.SpatRaster = function(learner, object, filename = NULL, overwrite = FALSE) {
  # read cell values from raster stack
  if (!is.null(filename)) {
    assert_path_for_output(filename, overwrite = overwrite)
    # we need to init the values with a factor class, otherwise setting the
    # values later on causes conversion troubles
    target_raster = terra::rast(terra::ext(object), res = terra::res(object),
      crs = terra::crs(object), vals = c("TRUE", "FALSE"))
  }
  terra::readStart(object)
  newdata_pred = as.data.table(terra::readValues(object, dataframe = TRUE))
  terra::readStop(object)

  pred = learner$predict_newdata(newdata_pred)

  if (!is.null(filename)) {
    target_raster = terra::setValues(target_raster, pred$response)
    terra::writeRaster(target_raster, filename, overwrite = overwrite)
  }
  return(pred)
}
