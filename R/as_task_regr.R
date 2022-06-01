#' @title Convert to a Regression Task
#'
#' @description
#' Convert object to a [TaskRegr].
#'
#' @param x (any)\cr
#'   Object to convert.
#' @param target (`character(1)`)\cr
#'   Name of the target column.
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of the data argument.
#' @param label (`character(1)`)\cr
#'   Label for the new instance.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @return [TaskRegr].
#' @rdname as_task_regr
#'
#' @export as_task_regr.sf
#' @exportS3Method
as_task_regr.sf = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, ...) {
  b = as_data_backend(x)
  if (is.null(target)) {
    Task$new(id = id, backend = b, task_type = "regr")
  } else {
    TaskRegr$new(id = id, backend = b, target = target, label = label)
  }
}

#' @rdname as_task_regr
#' @export as_task_regr.stars
#' @exportS3Method
as_task_regr.stars = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, ...) {
  b = as_data_backend(x)
  if (is.null(target)) {
    Task$new(id = id, backend = b, task_type = "regr")
  } else {
    TaskRegr$new(id = id, backend = b, target = target, label = label)
  }
}

#' @rdname as_task_regr
#' @export as_task_regr.SpatRaster
#' @exportS3Method
as_task_regr.SpatRaster = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, ...) {
  b = as_data_backend(x)
  if (is.null(target)) {
    Task$new(id = id, backend = b, task_type = "regr")
  } else {
    TaskRegr$new(id = id, backend = b, target = target, label = label)
  }
}

#' @rdname as_task_regr
#' @export as_task_regr.RasterBrick
#' @exportS3Method
as_task_regr.RasterBrick = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, ...) {
  b = as_data_backend(x)
  if (is.null(target)) {
    Task$new(id = id, backend = b, task_type = "regr")
  } else {
    TaskRegr$new(id = id, backend = b, target = target, label = label)
  }
}

#' @rdname as_task_regr
#' @export as_task_regr.Raster
#' @exportS3Method
as_task_regr.Raster = function(x, target = NULL, id = deparse(substitute(x)), label = NA_character_, ...) {
  b = as_data_backend(x)
  if (is.null(target)) {
    Task$new(id = id, backend = b, task_type = "regr")
  } else {
    TaskRegr$new(id = id, backend = b, target = target, label = label)
  }
}
