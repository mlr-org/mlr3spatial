#' @param chunksize `[integer]`\cr
#' The chunksize determines in how many subparts the prediction task will be
#' split into. The value can be roughly thought of as megabyte of a raster file
#' on disk. For example, if a prediction on a 1 GB file would be carried out
#' with `chunksize = 100L`, the prediction would happen in 10 chunks.
#'
#' The default of `chunksize = 1000L` might be a good compromise between speed
#' and memory usage. If you find yourself running out of memory, reduce this
#' value.
