#' @export as_task_classif.sf
#' @exportS3Method
#' @rdname as_data_backend
as_task_classif.sf = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, label = NA_character_, task_train = NULL, ...) {
  b = as_data_backend(x)
  TaskClassif$new(id = id, backend = b, target = target, positive = positive, label = label)
}

#' @export as_task_classif.SpatRaster
#' @exportS3Method
#' @rdname as_data_backend
as_task_classif.SpatRaster = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, label = NA_character_, task_train = NULL, ...) {
  b = as_data_backend(x, task_train = task_train)
  TaskClassif$new(id = id, backend = b, target = target, positive = positive, label = label)
}
