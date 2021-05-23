#' @export
demo_raster <- function(x, cells, dimension) {
  data <- matrix(c(stats::rnorm(cells / 2, 0, 1), stats::rnorm(cells / 2, 1, 1)),
    nrow = dimension
  )
  raster::raster(data, xmn = 0, xmx = dimension, ymn = 0, ymx = dimension)
}

#' @export
demo_stack <- function(size = 1000000000, layers = 5) {
  dimension <- floor(sqrt(size / 8 / layers))
  if (dimension %% 2 == 1) {
    dimension <- dimension - 1
  }
  cells <- dimension^2

  response <- matrix(rep(c(0, 1), each = cells / 2),
    nrow = dimension
  )
  response <- list(raster::raster(response, xmn = 0, xmx = dimension, ymn = 0, ymx = dimension))

  predictors <- lapply(seq(layers - 1), demo_raster, cells, dimension)

  do.call("stack", c(response, predictors))
}
