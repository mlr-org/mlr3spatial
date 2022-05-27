#' @title Generate Demo Raster
#'
#' @description
#' Generates a square demo [terra::SpatRaster]
#'
#' @param dimension (`integer(1)`)
#' xy dimension of raster
#'
#' @keywords internal
demo_raster = function(dimension) {
  assert_int(dimension, lower = 2)
  assert_int(dimension, lower = 2)
  data = matrix(c(stats::rnorm(floor(dimension^2 / 2), 0, 1),
    stats::rnorm(ceiling(dimension^2 / 2), 1, 1)), nrow = dimension)
  write_raster(data)
}

#' @title Generate a demo 'terra::SpatRaster'
#'
#' @description
#' Generates a square demo [terra::SpatRaster] object with options to set the
#' size (on disk) and number of layers.
#'
#' @param size `[integer(1)]`\cr
#'   Size of raster stack in megabyte (disk space).
#' @param layers `[integer(1)]`\cr
#'   Number of layers.
#'
#' @examples
#' demo_stack_spatraster(size = 5, layers = 2)
#' @export
demo_stack_spatraster = function(size = 50, layers = 5) {
  assert_int(layers, lower = 1)

  dimension = floor(sqrt(size / layers * 1e+06 / 8))
  raster_features = replicate(layers - 1, demo_raster(dimension))
  data_response = matrix(c(rep(0, floor(dimension^2 / 2)), rep(1, ceiling(dimension^2 / 2))), nrow = dimension)
  raster_response = write_raster(data_response)
  raster = terra::rast(c(raster_features, list(raster_response)))
  names(raster) = c(paste0("x_", 1:(layers - 1)), "y")
  raster
}

#' @title Generate a demo 'raster::brick'
#' @description
#' Generates a square demo [raster::brick] object with options to set the size
#' (on disk) and number of layers.
#' @inheritParams demo_stack_spatraster
#' @examples
#' if (mlr3misc::require_namespaces("raster", quietly = TRUE)) {
#'   demo_stack_rasterbrick(size = 5, layers = 2)
#' }
#' @export
demo_stack_rasterbrick = function(size = 50, layers = 5) {
  assert_int(layers, lower = 1)

  dimension = floor(sqrt(size / layers * 1e+06 / 4))
  raster_features = replicate(layers - 1, demo_rasterbrick(dimension))
  # NB: raster can't deal with factors, TRUE/FALSE will be converted into 0/1
  raster_response = raster::raster(matrix(c(rep(TRUE, floor(dimension^2 / 2)),
    rep(FALSE, ceiling(dimension^2 / 2))), nrow = dimension))
  brick = raster::brick(c(raster_features, list(raster_response)))
  names(brick) = c(paste0("x_", 1:(layers - 1)), "y")
  brick
}

demo_rasterbrick = function(dimension) {
  assert_int(dimension, lower = 2)
  data = matrix(c(stats::rnorm(floor(dimension^2 / 2), 0, 1),
    stats::rnorm(ceiling(dimension^2 / 2), 1, 1)), nrow = dimension)
  raster::raster(data)
}


#' @title Numeric Raster Layer
#'
#' @description
#' Creates a numeric raster layer when passed to `create_stack()`.
#'
#' @param id `character(1)`\cr
#'   Layer id.
#' @param in_memory `logical(1)`\cr
#'   If `FALSE` (default), layer is written to disk.
#'
#' @export
numeric_layer = function(id, in_memory = FALSE) {
  assert_string(id)
  assert_flag(in_memory)

  list(id = id, type = "numeric", in_memory = in_memory)
}

#' @title Categorical Raster Layer
#'
#' @description
#' Creates a categorical raster layer when passed to `create_stack()`.
#'
#' @param id `character(1)`\cr
#'   Layer id.
#' @param levels `character()`\cr
#'   Categories of the layer.
#' @param in_memory `logical(1)`\cr
#'   If `FALSE` (default), layer is written to disk.
#'
#' @export
factor_layer = function(id, levels, in_memory = FALSE) {
  assert_string(id)
  assert_character(levels)
  assert_flag(in_memory)

  list(id = id, type = "factor", levels = levels, in_memory = in_memory)
}

#' @title Raster Stack
#'
#' @description
#' Generates a square demo [terra::SpatRaster] from a list of layers.
#'
#' @param layers List of `numeric_layer()` and `factor_layer()`\cr
#'   List of layers.
#' @param layer_size `numeric(1)`\cr
#'   Size of a single layer in Megabytes.
#' @param dimension `integer(1)`\cr
#'   Number of rows and cols.
#'
#' `layer_size` and `dimension` are mutually exclusive.
#'
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
  set_names(rast(layers), ids)
}

#' @title Sample Stack
#'
#' @description
#' Samples a [terra::SpatRaster] and returns points.
#'
#' @param stack [terra::SpatRaster]\cr
#'   List of layers.
#' @param n `numeric(1)`\cr
#'   Number of sample points.
#'
#' @export
sample_vector = function(stack, n = 1000) {
  # workaround spatSample does not work with categorical layers
  extent = terra::ext(stack)
  layer_factor = names(stack)[terra::is.factor(stack)]
  points = terra::spatSample(extent, size = n, lonlat = FALSE, as.points = TRUE)
  data = terra::extract(stack, points)
  data$ID = NULL
  points = terra::setValues(points, data)
  vector = sf::st_as_sf(points)
  for (layer in layer_factor) {
    vector[layer] = factor(vector[[layer]])
  }
  vector
}
