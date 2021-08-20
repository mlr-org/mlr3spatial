#' @title Convert to and between spatial DataBackends
#' @description
#' S3 dispatch methods for converting from and to various spatial [mlr3::DataBackend]s.
#' @inheritParams mlr3::as_data_backend
#' @inherit mlr3::as_data_backend description title
#' @rdname as_data_backend
#' @return [DataBackend].
#' @seealso DataBackendSF DataBackendStars
#' @export
as_sf_backend = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  UseMethod("as_sf_backend")
}

#' @export
as_sf_backend.sf = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  assert_data_frame(data, min.cols = 1L, col.names = "unique")
  assert_class(data, "sf")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendSF$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @rdname as_data_backend
as_sf_backend.SpatRaster = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  spatvector = terra::as.polygons(data, trun = FALSE, dissolve = FALSE)
  data = sf::st_as_sf(spatvector)

  assert_data_frame(data, min.cols = 1L, col.names = "unique")
  assert_class(data, "sf")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendSF$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @rdname as_data_backend
as_sf_backend.RasterBrick = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint

  rasterbrick = raster::rasterToPolygons(data)
  data = sf::st_as_sf(rasterbrick)

  assert_data_frame(data, min.cols = 1L, col.names = "unique")
  assert_class(data, "sf")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendSF$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @param polygons `[logical]`\cr
#'   Whether to convert to polygons instead of points.
#' @rdname as_data_backend
as_sf_backend.DataBackendSpatRaster = function(data, primary_key = NULL, keep_rownames = FALSE, polygons = FALSE, ...) { # nolint

  assert_data_frame(data$head(), min.cols = 1L, col.names = "unique")

  if (polygons) {
    spatvector = terra::as.polygons(data$stack, trun = FALSE, dissolve = FALSE)
  } else {
    spatvector = terra::as.points(data$stack)
  }
  data = sf::st_as_sf(spatvector)

  assert_class(data, "sf")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendSF$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}

#' @export
#' @rdname as_data_backend
as_sf_backend.DataBackendRasterBrick = function(data, primary_key = NULL, keep_rownames = FALSE, polygons = FALSE, ...) { # nolint
  if (polygons) {
    vec = raster::rasterToPolygons(data$stack)
  } else {
    vec = raster::rasterToPoints(data$stack, spatial = TRUE)
  }
  sf_data = sf::st_as_sf(vec)
  if (data$response_is_factor) {
    sf_data[[data$response]] = as.factor(as.character(sf_data[[data$response]]))
  }
  data = sf_data

  assert_data_frame(data, min.cols = 1L, col.names = "unique")
  assert_class(data, "sf")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendSF$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}
