#' @description
#' Writes square raster to disk in chunks. Internal helper function.
write_raster = function(data) {
  # create temp file
  filename = tempfile(fileext = ".tif")
  # library(terra)
  # browser()
  # target_raster = terra::rast(ncols = nrow(data), nrows = nrow(data), xmin = 0,
  # xmax = nrow(data), ymin = 0, ymax = nrow(data))
  target_raster = terra::rast(data)
  # browser()
  # calculate block size
  bs = block_size(target_raster, 100)
  # initialize target raster
  terra::writeStart(target_raster, filename = filename)
  # browser()
  # write values in chunks
  pmap(list(bs$cells_seq, bs$cells_to_read), function(row, nrows) {
    terra::writeValues(target_raster,
      data[1:(terra::rowFromCell(target_raster, row) + terra::rowFromCell(target_raster, nrows) - 1),
        1:terra::nrow(data)], terra::rowFromCell(target_raster, row), terra::rowFromCell(target_raster, nrows))
  })
  terra::writeStop(target_raster)
  terra::rast(filename)
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
  assert_int(dimension, lower = 2)
  data = matrix(c(stats::rnorm(floor(dimension^2 / 2), 0, 1),
    stats::rnorm(ceiling(dimension^2 / 2), 1, 1)), nrow = dimension)
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

  dimension = floor(sqrt(size / layers * 1e+06 / 4))
  raster_features = replicate(layers - 1, demo_raster(dimension))
  data_response = matrix(c(rep(0, floor(dimension^2 / 2)), rep(1, ceiling(dimension^2 / 2))), nrow = dimension)
  raster_response = write_raster(data_response)
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
#' @export
block_size = function(raster, chunksize) {
  assert_class(raster, "SpatRaster")
  # chunksize in bytes
  chunksize = assert_numeric(chunksize) * 1e+06

  # row_size in bites; one cell takes 8 byte memory
  row_size = terra::ncol(raster) * terra::nlyr(raster) * 8
  # # browser()
  # Hom many rows can be processed in one block?
  nrow_block = chunksize / row_size
  # How many cells are this?
  ncells_block = nrow_block * terra::ncol(raster)
  # split all cells by ncells_block
  cells_seq = seq(1, terra::ncell(raster), by = ncells_block)
  # number of rows to read per block
  cells_to_read = rep(ncells_block, length(cells_seq))
  # browser()

  cells_to_read[length(cells_to_read)] = terra::ncell(raster) - cells_seq[length(cells_seq)] + 1

  # # number of rows in one block
  # nrow = chunksize / row_size
  # # start row indices
  # nrow_seq = seq(1, terra::nrow(raster), by = nrow)
  # # number of rows to read per block
  # rows_to_write = rep(nrow, length(nrow_seq))
  # rows_to_write[length(rows_to_write)] = terra::nrow(raster) - nrow_seq[length(nrow_seq)] + 1

  # browser()

  # one cell is 8 bytes
  # cellsize = 0.000008
  # ncell = terra::ncell(raster)

  # number of rows in one block
  # # cell_size = terra::ncell(raster) * 8
  # row_size = terra::ncol(raster) * terra::nlyr(raster) * 8
  # # ncells_chunks = chunksize / cellsize
  # # start row indices
  # row = seq(1, terra::ncell(raster), by = row_size)
  # # # number of rows to read per block
  # # nrows = rep(nrow, length(row))
  # # nrows[length(nrows)] = terra::nrow(raster) - row[length(row)] + 1

  return(list(cells_seq = cells_seq, cells_to_read = cells_to_read # ,
    # nrow_seq = nrow_seq, rows_to_write = rows_to_write
  ))
}

stack_classif = demo_stack(1)
value = data.table(ID = c(0, 1), y = c("negative", "positive"))
terra::setCats(stack_classif, layer = "y", value = value)
