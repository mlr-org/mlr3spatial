#' @inheritParams mlr3::as_data_backend
#' @rdname as_data_backend
#' @template param-quiet
#' @export
as_stars_backend = function(data, primary_key = NULL, response = NULL,
  response_is_factor = FALSE, quiet = FALSE, ...) { # nolint
  UseMethod("as_stars_backend")
}

#' @export
as_stars_backend.stars = function(data, primary_key = NULL,
  response = NULL, response_is_factor = FALSE, quiet = FALSE, ...) { # nolint
  assert_class(data, "stars")

  b = DataBackendStars$new(data, primary_key, response = response,
    response_is_factor = response_is_factor, quiet = quiet)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @rdname as_data_backend
as_stars_backend.SpatRaster = function(data, primary_key = NULL,
  response = NULL, response_is_factor = FALSE, quiet = FALSE, ...) { # nolint
  data = stars::st_as_stars(data)
  assert_class(data, "stars")

  b = DataBackendStars$new(data, primary_key,
    response = response, response_is_factor = response_is_factor, quiet = quiet)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @rdname as_data_backend
as_stars_backend.RasterBrick = function(data, primary_key = NULL,
  response = NULL, response_is_factor = FALSE, quiet = FALSE, ...) { # nolint

  data = stars::st_as_stars(data)
  assert_class(data, "stars")

  b = DataBackendStars$new(data, primary_key, response = response,
    response_is_factor = response_is_factor, quiet = quiet)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @param polygons `[logical]`\cr
#'   Whether to convert to polygons instead of points.
#' @rdname as_data_backend
as_stars_backend.DataBackendSpatRaster = function(data, primary_key = NULL,
  response = NULL, response_is_factor = FALSE, quiet = FALSE, ...) { # nolint

  data = stars::st_as_stars(data$stack)

  assert_class(data, "stars")

  b = DataBackendStars$new(data, primary_key, response = response,
    response_is_factor = response_is_factor, quiet = quiet)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @rdname as_data_backend
as_stars_backend.DataBackendRasterBrick = function(data, primary_key = NULL,
  response = NULL, response_is_factor = FALSE, quiet = FALSE, ...) { # nolint

  data = stars::st_as_stars(data$stack, quiet = quiet)
  assert_class(data, "stars")

  b = DataBackendStars$new(data, primary_key, response = response,
    response_is_factor = response_is_factor, quiet = quiet)
  b$compact_seq = FALSE

  return(b)
}
