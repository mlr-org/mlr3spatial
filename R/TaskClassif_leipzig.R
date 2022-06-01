#' @title Leipzig Land Cover Task
#'
#' @name mlr_tasks_leipzig
#'
#' @description
#' Sentinel-2 spectral bands and NDVI to predict land cover in Leipzig.
#'
#' @source
#' Sentinel-2
#'
#' @docType data
#' @keywords data
#' @examples
#' data("kc_housing", package = "mlr3data")
#' str(kc_housing)
NULL

load_task_leipzig = function(id = "leipzig") {
  b = as_data_backend(sf::read_sf(system.file("extdata", "leipzig_points.gpkg", package = "mlr3spatial"), stringsAsFactors = TRUE))
  task = TaskClassif$new(id, b, target = "land_cover", label = "Leipzig Land Cover")
  b$hash = task$man = "mlr3::mlr_tasks_leipzig"
  task
}
