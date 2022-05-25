# creates a numeric layer when passed to create_stack
numeric_layer = function(id, in_memory = FALSE) {
  assert_string(id)
  assert_flag(in_memory)

  list(id = id, type = "numeric", in_memory = in_memory)
}

# creates a factor layer when passed to create_stack
factor_layer = function(id, levels, in_memory = FALSE) {
  assert_string(id)
  assert_character(levels)
  assert_flag(in_memory)

  list(id = id, type = "factor", levels = levels, in_memory = in_memory)
}

# creates a stack
create_stack = function(layers, layer_size = NULL, dimension = NULL) {
  if (!xor(is.null(layer_size), is.null(dimension))) {
    stop("Either `layer_size` or `dimension` must be provided")
  }
  assert_list(layers)
  assert_int(layer_size, null.ok = TRUE)
  assert_int(dimension, null.ok = TRUE)
  dimension = dimension %??% floor(sqrt(layer_size * 1e+06))
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

create_vector = function(stack, n = 1000) {
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

add_aoi = function(stack) {
  val = terra::values(stack[[1]])
  val[seq(nrow(val) * 0.1)] = NA_real_
  stack = terra::setValues(stack[[1]], val)
  stack
}
