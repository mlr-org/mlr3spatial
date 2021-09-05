#' Writes a raster in chunks
#' @description
#' Writes square raster to disk in chunks. Internal helper function.
#' @param data `[SpatRaster]`\cr
#'   `SpatRaster` object.
#' @keywords internal
#' @export
write_raster = function(data) {
  # create temp file
  filename = tempfile(fileext = ".tif")
  target_raster = terra::rast(data)
  # calculate block size
  bs = block_size(target_raster, 100)
  # initialize target raster
  terra::writeStart(target_raster, filename = filename)
  # write values in chunks
  mlr3misc::pmap(list(bs$cells_seq, bs$cells_to_read), function(row, nrows) {
    terra::writeValues(target_raster,
      data[1:(terra::rowFromCell(target_raster, row) + terra::rowFromCell(target_raster, nrows) - 1),
        1:terra::nrow(data)], terra::rowFromCell(target_raster, row), terra::rowFromCell(target_raster, nrows))
  })
  terra::writeStop(target_raster)
  terra::rast(filename)
}

#' @title Split Raster Into Chunks
#'
#' @description
#' Splits raster into chunks.
#'
#' @param raster ([terra::SpatRaster])\cr
#' Raster to be split into chunks.
#' @template param-chunksize
#'
#' @export
#' @keywords internal
block_size = function(raster, chunksize) {
  assert_class(raster, "SpatRaster")
  # chunksize in bytes
  chunksize = assert_numeric(chunksize) * 1e+06

  # row_size in bites; one cell takes 8 byte memory
  row_size = terra::ncol(raster) * terra::nlyr(raster) * 8
  # Hom many rows can be processed in one block?
  nrow_block = chunksize / row_size
  # How many cells are this?
  ncells_block = nrow_block * terra::ncol(raster)
  # split all cells by ncells_block
  cells_seq = seq(1, terra::ncell(raster), by = ncells_block)
  # number of rows to read per block
  cells_to_read = rep(ncells_block, length(cells_seq))

  cells_to_read[length(cells_to_read)] = terra::ncell(raster) - cells_seq[length(cells_seq)] + 1

  return(list(cells_seq = cells_seq, cells_to_read = cells_to_read))
}
