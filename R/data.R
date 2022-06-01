#' @title Numeric Layer Generator
#'
#' @description
#' Generates a numeric layer when passed to [generate_stack()].
#'
#' @param id (`character(1)`)\cr
#'   Layer id.
#' @param in_memory (`logical(1)`)\cr
#'   If `FALSE` (default), layer is written to disk.
#'
#' @return Named `list()`
#'
#' @keywords internal
#' @export
numeric_layer = function(id, in_memory = FALSE) {
  assert_string(id)
  assert_flag(in_memory)

  list(id = id, type = "numeric", in_memory = in_memory)
}

#' @title Factor Layer Generator
#'
#' @description
#' Generates a factor layer when passed to [generate_stack()].
#'
#' @param id (`character(1)`)\cr
#'   Layer id.
#' @param levels (`character()`)\cr
#'   Factor levels.
#' @param in_memory (`logical(1)`)\cr
#'   If `FALSE` (default), layer is written to disk.
#'
#' @return Named `list()`
#'
#' @keywords internal
#' @export
factor_layer = function(id, levels, in_memory = FALSE) {
  assert_string(id)
  assert_character(levels)
  assert_flag(in_memory)

  list(id = id, type = "factor", levels = levels, in_memory = in_memory)
}

#' @title Generate Raster Stack
#'
#' @description
#' Generates a raster stack.
#'
#' @param layers (List of [numeric_layer()] and [factor_layer()])\cr
#'   List of layers.
#' @param layer_size (`numeric(1)`)\cr
#'   Size of a single layer in megabytes.
#' @param dimension (`integer(1)`)\cr
#'   Dimension of the squared layers.
#'
#' `layer_size` and `dimension` are mutually exclusive.
#'
#' @return [terra::SpatRaster]
#'
#' @keywords internal
#' @export
generate_stack = function(layers, layer_size = NULL, dimension = NULL) {
  if (!xor(is.null(layer_size), is.null(dimension))) {
    stop("Either `layer_size` or `dimension` must be provided")
  }
  assert_list(layers)
  assert_int(layer_size, null.ok = TRUE)
  assert_int(dimension, null.ok = TRUE)
  dimension = dimension %??% floor(sqrt(layer_size * 1e+06 / 8))
  ids = map_chr(layers, "id")
  assert_character(ids, unique = TRUE)

  layers = map(layers, function(layer) {
    if (layer$type == "numeric") {
      data = matrix(c(stats::rnorm(floor(dimension^2 / 2), 0, 1), stats::rnorm(ceiling(dimension^2 / 2), 1, 1)), nrow = dimension)
      ras = rast(data)
      if (!layer$in_memory) {
        filename = tempfile(fileext = ".tif")
        writeRaster(ras, filename)
        ras = rast(filename)
      }
      ras
    } else if (layer$type == "factor") {
      data = matrix(rep(seq_along(layer$levels), each = floor(dimension^2 / length(layer$levels)), length.out = dimension^2), nrow = dimension)
      ras = rast(data)

      ras = terra::categories(ras, layer = 1, data.table(ID = seq_along(layer$levels), category = layer$levels))
      if (!layer$in_memory) {
        filename = tempfile(fileext = ".tif")
        writeRaster(ras, filename)
        ras = rast(filename)
      }
      ras
    }
  })
  stack = rast(layers)
  terra::crs(stack) = "EPSG:4326"
  set_names(stack, ids)
}

#' @title Sample Points in Raster Stack
#'
#' @description
#' Samples `n` points of a raster stack.
#'
#' @param stack ([terra::SpatRaster])\cr
#'   Raster stack.
#' @param n (`integer(1)`)\cr
#'   Number of points.
#'
#' @return [sf::sf]
#'
#' @keywords internal
#' @export
sample_stack = function(stack, n = 100) {
  # WORKAROUND: spatSample fails with categorical layers
  extent = terra::ext(stack)
  layer_factor = names(stack)[terra::is.factor(stack)]
  points = terra::spatSample(extent, size = n, lonlat = FALSE, as.points = TRUE)
  data = terra::extract(stack, points)
  data$ID = NULL
  points = terra::setValues(points, data)
  vector = sf::st_as_sf(points)
  vector = sf::st_set_crs(vector, "EPSG:4326")
  for (layer in layer_factor) {
    vector[layer] = factor(vector[[layer]])
  }
  vector
}

#' @title Sample Points in Raster Stack
#'
#' @description
#' Masks stack to a circular area of interest.
#'
#' @param stack ([terra::SpatRaster])\cr
#'   Raster stack.
#' @param n (`integer(1)`)\cr
#'   Number of points.
#'
#' @return [terra::SpatRaster]
#'
#' @keywords internal
#' @export
mask_stack = function(stack) {
  x = (terra::xmax(stack) - terra::xmin(stack)) / 2
  y = (terra::xmax(stack) - terra::ymin(stack)) / 2
  point = sf::st_as_sf(sf::st_as_sfc(list(sf::st_point(c(x, y)))))
  polygon = sf::st_buffer(point, dist = x * 0.8)
  mask = terra::vect(polygon)

  terra::mask(stack, mask)
}
