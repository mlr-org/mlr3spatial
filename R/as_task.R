#' @export
as_task.SpatRaster = function(x, id = deparse(substitute(x)), label = NA_character_, ...) {
  Task$new(id = id, backend = x, label = label)
}
