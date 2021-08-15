#' @title Generate Demo Raster
#'
#' @description
#' Generates a square demo [terra::SpatRaster].
#'
#' @param dimension (`integer(1)`)
#' xy dimension of raster
#'
#' @importFrom terra rast
#' @keywords internal
demo_raster = function(dimension) {
  assert_int(dimension, lower = 2)
  data = matrix(c(stats::rnorm(floor(dimension^2 / 2), 0, 1),
    stats::rnorm(ceiling(dimension^2 / 2), 1, 1)), nrow = dimension)
  terra::rast(data)
}

#' @title Generate a dummy 'terra::SpatRaster'
#'
#' @description
#' Generates a square demo [terra::SpatRaster] stack.
#'
#' @param size `[integer(1)]`\cr
#'   Size of raster stack in megabyte (disk space).
#' @param layers `[integer(1)]`\cr
#'   Number of layers.
#'
#' @importFrom terra rast
#' @export
demo_stack = function(size = 50, layers = 5) {
  assert_int(size, lower = 1)
  assert_int(layers, lower = 1)

  dimension = floor(sqrt(size / layers * 1e+06 / 4))
  raster_features = replicate(layers - 1, demo_raster(dimension))
  raster_response = terra::rast(matrix(c(rep("FALSE", floor(dimension^2 / 2)),
    rep("TRUE", ceiling(dimension^2 / 2))), nrow = dimension))
  raster = rast(c(raster_features, list(raster_response)))
  names(raster) = c(paste0("x_", 1:(layers - 1)), "y")
  raster
}

demo_stack_raster = function(size = 50, layers = 5) {
  assert_int(size, lower = 1)
  assert_int(layers, lower = 1)

  dimension = floor(sqrt(size / layers * 1e+06 / 4))
  raster_features = replicate(layers - 1, demo_raster_raster(dimension))
  raster_response = raster::raster(matrix(c(rep(FALSE, floor(dimension^2 / 2)),
    rep(TRUE, ceiling(dimension^2 / 2))), nrow = dimension))
  raster = raster::brick(c(raster_features, list(raster_response)))
  names(raster) = c(paste0("x_", 1:(layers - 1)), "y")
  raster
}

demo_raster_raster = function(dimension) {
  assert_int(dimension, lower = 2)
  data = matrix(c(stats::rnorm(floor(dimension^2 / 2), 0, 1),
    stats::rnorm(ceiling(dimension^2 / 2), 1, 1)), nrow = dimension)
  raster::raster(data)
}
