#' @title Convert to a Classification Task
#'
#' @description
#' Convert object to a [mlr3::TaskClassif].
#'
#' @param x (any)\cr
#'   Object to convert.
#' @param target (`character(1)`)\cr
#'   Name of the target column.
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of the data argument.
#' @param positive (`character(1)`)\cr
#'   Level of the positive class. See [TaskClassif].
#' @param label (`character(1)`)\cr
#'   Label for the new instance.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @return [TaskClassif].
#' @rdname as_task_classif
#'
#' @export as_task_classif.sf
#' @exportS3Method
as_task_classif.sf = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, label = NA_character_, ...) {
  b = as_data_backend(x)
  if (is.null(target)) {
    Task$new(id = id, backend = b, task_type = "classif", label = label)
  } else {
    TaskClassif$new(id = id, backend = b, target = target, positive = positive, label = label)
  }
}

#' @rdname as_task_classif
#' @export as_task_classif.SpatRaster
#' @exportS3Method
as_task_classif.SpatRaster = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, label = NA_character_, ...) {
  b = as_data_backend(x)
  if (is.null(target)) {
    Task$new(id = id, backend = b, task_type = "classif", label = label)
  } else {
    TaskClassif$new(id = id, backend = b, target = target, positive = positive, label = label)
  }
}
