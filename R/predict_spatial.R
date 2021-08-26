#' @title Predict on spatial objects with mlr3 learners
#' @description
#' This function allows to directly predict mlr3 learners on various spatial
#' xs (see section "Supported Spatial Classes"). It returns an
#' [mlr3::Prediction] x and (optionally) the same x that was used for
#' the prediction.
#' @param x `[SpatRaster, sf, RasterBrick]`\cr
#'   Object to predict on which is treated as "newdata".
#' @param task ([Task]) For internal use only.
#' @param self (`any`)\cr
#'   For internal use only.
#' @param ... Passed down to the respective predict method. See section
#'  "Additional arguments".
#'
#' @section Additional arguments:
#'
#' - `filename` `[character]`\cr
#'   Filename of optional file to write prediction values into.
#'   For raster-like inputs this can be a `.tif` file.
#'   For {sf} xs, this could be a `.gpgk` or a `.shp` file.
#' - `overwrite` `[logical]`\cr
#'   Should a possibly existing file on disk (referring to argument `filename`)
#'   be overwritten?
#' - `quiet` `[logical]`\cr
#'   Whether to suppress possible console output when invoking the writing
#'   methods of the respective spatial classes.
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
#' @return mlr3::Prediction
#' @examples
#' if (mlr3misc::require_namespaces(c("terra", "future"), quietly = TRUE)) {
#'   stack = demo_stack_spatraster(size = 5, layers = 5)
#'   backend = DataBackendSpatRaster$new(stack)
#'   task = as_task_classif(backend, target = "y", positive = "TRUE")
#'   # train
#'   learner = lrn("classif.featureless")
#'   learner$train(task, row_ids = sample(1:task$nrow, 500))
#'   learner$predict_newdata(stack)
#' }
#' @export
#' @rdname predict_newdata_s3
predict_newdata_s3.SpatRaster = function(x, task = NULL, self = NULL, ...) {

  # handle additional arguments
  ell = list(...)
  filename = ell$filename
  overwrite = ell$overwrite
  quiet = ell$quiet
  if (is.null(overwrite)) {
    overwrite = FALSE
  }
  if (is.null(quiet)) {
    quiet = FALSE
  }

  # read cell values from raster stack
  if (!is.null(filename)) {
    assert_path_for_output(filename, overwrite = overwrite)
    # we need to init the values with a factor class, otherwise setting the
    # values later on causes conversion troubles
    target_raster = terra::rast(terra::ext(x), res = terra::res(x),
      crs = terra::crs(x), vals = c("TRUE", "FALSE"))
  }
  terra::readStart(x)
  newdata_pred = as.data.table(terra::readValues(x, dataframe = TRUE))
  terra::readStop(x)

  pred = self$predict_newdata(newdata_pred)

  if (!is.null(filename)) {
    target_raster = terra::setValues(target_raster, pred$response)
    terra::writeRaster(target_raster, filename, overwrite = overwrite)
  }
  return(pred)
}

#' @export
#' @rdname predict_newdata_s3
predict_newdata_s3.RasterBrick = function(x, task = NULL, self = NULL, ...) {

  # handle additional arguments
  ell = list(...)
  filename = ell$filename
  overwrite = ell$overwrite
  quiet = ell$quiet
  if (is.null(overwrite)) {
    overwrite = FALSE
  }
  if (is.null(quiet)) {
    quiet = FALSE
  }

  # read cell values from raster stack
  if (!is.null(filename)) {
    assert_path_for_output(filename, overwrite = overwrite)
    # we need to init the values with a factor class, otherwise setting the
    # values later on causes conversion troubles
    target_raster = raster::raster(nrows = raster::nrow(x),
      ncols = raster::ncol(x),
      crs = raster::crs(x))
  }
  raster::readStart(x)
  newdata_pred = as.data.table(raster::getValues(x))
  raster::readStop(x)

  pred = self$predict_newdata(newdata_pred)

  if (!is.null(filename)) {
    target_raster = raster::setValues(target_raster, pred$response)
    raster::writeRaster(target_raster, filename, overwrite = overwrite)
  }
  return(pred)
}

#' @export
#' @rdname predict_newdata_s3
predict_newdata_s3.sf = function(x, task = NULL, self = NULL, ...) {

  # handle additional arguments
  ell = list(...)
  filename = ell$filename
  overwrite = ell$overwrite
  quiet = ell$quiet
  if (is.null(overwrite)) {
    overwrite = FALSE
  }
  if (is.null(quiet)) {
    quiet = FALSE
  }

  if (!is.null(filename)) {
    assert_path_for_output(filename, overwrite = overwrite)
  }
  newdata_pred = as.data.table(x)
  newdata_pred$geometry = NULL
  attr(newdata_pred, "sf_column") = NULL
  attr(newdata_pred, "agr") = NULL

  pred = self$predict_newdata(newdata_pred)

  if (!is.null(filename)) {
    sf_pred = sf::st_as_sf(data.frame(pred = pred$response, geometry = x$geometry))
    sf::st_write(sf_pred, filename, quiet = quiet)
  }
  return(pred)
}

#' @export
#' @rdname predict_newdata_s3
predict_newdata_s3.stars = function(x, task = NULL, self = NULL, ...) {

  # handle additional arguments
  ell = list(...)
  filename = ell$filename
  overwrite = ell$overwrite
  quiet = ell$quiet
  if (is.null(overwrite)) {
    overwrite = FALSE
  }
  if (is.null(quiet)) {
    quiet = FALSE
  }

  if (!is.null(filename)) {
    assert_path_for_output(filename, overwrite = overwrite)
  }
  newdata_pred = as.data.table(split(x, "band"))

  if (any(c("x", "y") %in% colnames(newdata_pred))) { # nocov start
    if (!quiet) {
      messagef("Dropping coordinates 'x' and 'y' as they are
        most likely coordinates. If you want to have these variables included,
        duplicate them in the stars xs using a different name.
        To silence this message, set 'quiet = TRUE'.", wrap = TRUE)
      # nocov end
    }
    newdata_pred[, c("x", "y")] = list(NULL)
  }

  pred = self$predict_newdata(newdata_pred)
  # single band x
  stars_pred = x[, , , 1]

  stars::st_as_stars(dimensions = stars::st_dimensions(x))

  names(stars_pred) = "pred"
  stars_pred$pred = pred$response

  if (!is.null(filename)) {
    stars::write_stars(stars_pred, filename, quiet = quiet)
  }
  return(pred)
}
