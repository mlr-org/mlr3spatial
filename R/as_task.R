#' @export
as_task.RasterBrick = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  Task$new(id = id, backend = x, label = label, task_type = "classif")
}

#' @export
as_task.RasterStack = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  Task$new(id = id, backend = x, label = label, task_type = "classif")
}

#' @export
as_task.sf = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  Task$new(id = id, backend = x, label = label, task_type = "classif")
}

#' @export
as_task.SpatRaster = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  Task$new(id = id, backend = x, label = label, task_type = "classif")
}

#' @export
as_task.stars = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  Task$new(id = id, backend = x, label = label, task_type = "classif")
}
