#' @inheritParams mlr3::as_data_backend
#' @rdname as_data_backend
#' @export
as_stars_backend = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  UseMethod("as_stars_backend")
}

#' @export
as_stars_backend.stars = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  assert_class(data, "stars")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendStars$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @rdname as_data_backend
as_stars_backend.SpatRaster = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  data = stars::st_as_stars(data)

  assert_class(data, "stars")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendStars$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @rdname as_data_backend
as_stars_backend.RasterBrick = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint

  data = stars::st_as_stars(data)

  assert_class(data, "stars")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendStars$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @param polygons `[logical]`\cr
#'   Whether to convert to polygons instead of points.
#' @rdname as_data_backend
as_stars_backend.DataBackendSpatRaster = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint

  data = stars::st_as_stars(data$stack)

  assert_class(data, "stars")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendStars$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @rdname as_data_backend
as_stars_backend.DataBackendRasterBrick = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint

  data = stars::st_as_stars(data$stack)

  assert_class(data, "stars")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendStars$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}
