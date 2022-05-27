#' @export as_task_regr.sf
#' @exportS3Method
as_task_regr.sf = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, task_train = NULL, ...) {
  b = as_data_backend(x)
  TaskRegr$new(id = id, backend = b, target = target, label = label)
}

#' @export as_task_regr.stars
#' @exportS3Method
as_task_regr.stars = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, task_train = NULL, ...) {
  b = as_data_backend(x, task_train = task_train)
  TaskRegr$new(id = id, backend = b, target = target, label = label)
}

#' @export as_task_regr.SpatRaster
#' @exportS3Method
as_task_regr.SpatRaster = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, task_train = NULL, ...) {
  b = as_data_backend(x, task_train = task_train)
  TaskRegr$new(id = id, backend = b, target = target, label = label)
}

#' @export as_task_regr.RasterBrick
#' @exportS3Method
as_task_regr.RasterBrick = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, task_train = NULL, ...) {
  b = as_data_backend(x, task_train = task_train)
  TaskRegr$new(id = id, backend = b, target = target, label = label)
}

#' @export as_task_regr.Raster
#' @exportS3Method
as_task_regr.Raster = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, task_train = NULL, ...) {
  b = as_data_backend(x, task_train = task_train)
  TaskRegr$new(id = id, backend = b, target = target, label = label)
}
