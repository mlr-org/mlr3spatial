#' @title Convert to sf backend
#' @description
#' Converts to a [DataBackendSF].
#' @inheritParams mlr3::as_data_backend
#' @inherit mlr3::as_data_backend description title
#' @rdname as_data_backend
#' @return [DataBackend].
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
#' @rdname as_data_backend
as_sf_backend.DataBackendSpatRaster = function(data, primary_key = NULL, keep_rownames = FALSE, polygons = FALSE, ...) { # nolint
  # FIXME: https://github.com/rspatial/terra/issues/306
  # FIXME: https://github.com/rspatial/terra/issues/307
  # FIXME: NOT WORKING!
  # for some reasons (can't reproduce), we first need to read the values into a matrix
  # and then coerce to a DT
  # both, directly calling as.polygons() and reading as dataframe
  # (readValues(dataframe = TRUE)) lead to NA's.
  # tmp = terra::readValues(data$stack, mat = TRUE)
  # tmp2 = as.data.table(tmp)
  # spatvector = terra::as.polygons(data$stack, trun = FALSE, dissolve = FALSE)
  # spatpoints = terra::as.points(data$stack)
  # data = sf::st_as_sf(spatvector)
  # data = sf::st_polygon(tmp2, crs = terra::crs(data$stack))

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
