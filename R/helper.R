#' @description 
#' Writes square raster to disk in chunks. Internal helper function.
write_raster = function(data) {
  # create temp file
  browser()
  filename = tempfile(fileext = ".tif")
  target_raster = rast(ncols = nrow(data), nrows = nrow(data), xmin = 0, xmax = nrow(data), ymin = 0, ymax = nrow(data))
  # calculate block size
  bs = block_size(target_raster, 100)
  # initialize target raster
  writeStart(target_raster, filename = filename)
  # write values in chunks
  pmap(list(bs$row, bs$nrows), function(row, nrows) {
    terra::writeValues(target_raster, data[row:(row+nrows-1), 1:nrow(data)], row, nrows)
  })
  writeStop(target_raster)
  rast(filename)
}


#' @title Generate Demo Raster
#'
#' @description
#' Generates a square demo [terra::SpatRaster].
#'
#' @param dimension (`integer(1)`)
#' xy dimension of raster
#'
#' @export
demo_raster = function(dimension) {
  assert_int(dimension, lower = 2)
  data = matrix(c(stats::rnorm(floor(dimension^2 / 2), 0, 1), stats::rnorm(ceiling(dimension^2 / 2), 1, 1)), nrow = dimension)
  write_raster(data)
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
#' @export
demo_stack = function(size = 500, layers = 5) {
  assert_int(size, lower = 1)
  assert_int(layers, lower = 1)

  dimension = floor(sqrt(size / layers * 1e+06 / 8))
  raster_features = replicate(layers - 1, demo_raster(dimension))
  data_response = matrix(c(rep(0, floor(dimension^2 / 2)), rep(1, ceiling(dimension^2 / 2))), nrow = dimension)
  raster_response = write_raster(data_response)
  raster = rast(c(raster_features, list(raster_response)))
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
#' @export
block_size = function(raster, chunksize) {
  assert_class(raster, "SpatRaster")
  chunksize = assert_numeric(chunksize) * 1e+06

  # one cell takes 8 byte memory
  row_size = ncol(raster) * nlyr(raster) * 8
  # number of rows in one block
  nrow = chunksize / row_size
  # start row indices
  row = seq(1, nrow(raster), by = nrow)
  # number of rows to read per block
  nrows = rep(nrow, length(row))
  nrows[length(nrows)] = nrow(raster) - row[length(row)] + 1


  return(list(row = row, nrows = nrows))
}