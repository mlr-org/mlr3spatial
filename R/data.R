#' @title Generate Demo Raster
#'
#' @description
#' Generates a square demo [terra::SpatRaster].
#'
#' @param dimension (`integer(1)`)
#' xy dimension of raster
#'
#' @keywords internal
demo_spatraster = function(dimension) {
  assert_int(dimension, lower = 2)
  data = matrix(c(stats::rnorm(floor(dimension^2 / 2), 0, 1),
    stats::rnorm(ceiling(dimension^2 / 2), 1, 1)), nrow = dimension)
  terra::rast(data)
}

#' @title Generate a dummy 'terra::SpatRaster'
#'
#' @description
#' Generates a square demo [terra::SpatRaster] object.
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
  assert_int(size, lower = 1)
  assert_int(layers, lower = 1)

  dimension = floor(sqrt(size / layers * 1e+06 / 4))
  raster_features = replicate(layers - 1, demo_spatraster(dimension))
  raster_response = terra::rast(matrix(c(rep("FALSE", floor(dimension^2 / 2)),
    rep("TRUE", ceiling(dimension^2 / 2))), nrow = dimension))
  raster = terra::rast(c(raster_features, list(raster_response)))
  names(raster) = c(paste0("x_", 1:(layers - 1)), "y")
  raster
}

#' @title Generate a dummy 'raster::brick'
#' @description
#' Generates a square demo [raster::brick] object.
#' @inheritParams demo_stack_spatraster
#' @examples
#' if (mlr3misc::require_namespaces("raster", quietly = TRUE)) {
#'   demo_stack_rasterbrick(size = 5, layers = 2)
#' }
#' @export
demo_stack_rasterbrick = function(size = 50, layers = 5) {
  assert_int(size, lower = 1)
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
