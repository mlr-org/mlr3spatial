#' @title Generate Demo Raster
#'
#' @description
#' Generates a square demo [terra::SpatRaster].
#'
#' @param dimension (`integer(1)`)
#' xy dimension of raster
#'
#' @importFrom terra rast
#' @export
demo_raster = function(dimension) {
  assert_int(dimension, lower = 2)
  data = matrix(c(stats::rnorm(floor(dimension^2 / 2), 0, 1),
    stats::rnorm(ceiling(dimension^2 / 2), 1, 1)), nrow = dimension)
  terra::rast(data)
}

#' @title Generate Demo Raster Stack
#'
#' @description
#' Generates a square demo [terra::SpatRaster] stack.
#'
#' @param size (`integer(1)`)\cr
#' Size of raster stack in megabyte.
#' @param layers (`integer(1)`)\cr
#' Number of layers.
#'
#' @importFrom terra rast
#' @export
demo_stack = function(size = 500, layers = 5) {
  assert_int(size, lower = 1)
  assert_int(layers, lower = 1)

  dimension = floor(sqrt(size / layers * 1e+06 / 8))
  raster_features = replicate(layers - 1, demo_raster(dimension))
  raster_response = rast(matrix(c(rep(0, floor(dimension^2 / 2)),
    rep(1, ceiling(dimension^2 / 2))), nrow = dimension))
  raster = terra::rast(c(raster_features, list(raster_response)))
  names(raster) = c(paste0("x_", 1:(layers - 1)), "y")
  raster
}


#' @title Split Raster Into Chunks
#'
#' @description
#' Splits raster into chunks.
#'
#' @param raster ([terra::SpatRaster])\cr
#' Raster to be split into chunks.
#' @param chunksize
#' Raster chunk size in megabyte.
#'
#' @importFrom terra nlyr
#' @export
block_size = function(raster, chunksize) {
  assert_class(raster, "SpatRaster")
  chunksize = assert_numeric(chunksize) * 1e+06

  n = terra::nlyr(raster)
  blockrows = 1
  nr = nrow(raster)

  size = min(nr, max(1, floor(chunksize / (ncol(raster) * n * 8))))
  nb = ceiling(nr / size)
  row = (0:(nb - 1)) * size + 1
  nrows = rep(size, length(row))
  dif = nb * size - nr
  nrows[length(nrows)] = nrows[length(nrows)] - dif

  return(list(row = row, nrows = nrows, n = nb))
}
