#' @inheritParams mlr3::as_data_backend
#' @rdname as_data_backend
#' @template param-quiet
#' @export
as_spatraster_backend = function(data, primary_key = NULL, ...) { # nolint
  UseMethod("as_spatraster_backend")
}

#' @export
as_spatraster_backend.SpatRaster = function(data, primary_key = NULL, ...) { # nolint
  assert_class(data, "SpatRaster")

  b = DataBackendSpatRaster$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}
