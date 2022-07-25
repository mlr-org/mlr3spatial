#' @export
as_task_unsupervised.RasterBrick = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  TaskUnsupervised$new(id = id, backend = x, label = label)
}

#' @export
as_task_unsupervised.RasterStack = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  TaskUnsupervised$new(id = id, backend = x, label = label)
}

#' @export
as_task_unsupervised.SpatRaster = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  TaskUnsupervised$new(id = id, backend = x, label = label)
}

#' @export
as_task_unsupervised.stars = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  TaskUnsupervised$new(id = id, backend = x, label = label)
}
