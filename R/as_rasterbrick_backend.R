#' @inheritParams mlr3::as_data_backend
#' @rdname as_data_backend
#' @template param-quiet
#' @export
as_rasterbrick_backend = function(data, primary_key = NULL, response,
  response_is_factor = FALSE, ...) { # nolint
  UseMethod("as_rasterbrick_backend")
}

#' @export
as_rasterbrick_backend.RasterBrick = function(data, primary_key = NULL,
  response, response_is_factor = FALSE, ...) { # nolint
  assert_class(data, "RasterBrick")

  b = DataBackendRasterBrick$new(data, primary_key, response = response,
    response_is_factor = response_is_factor)
  b$compact_seq = FALSE

  return(b)
}
