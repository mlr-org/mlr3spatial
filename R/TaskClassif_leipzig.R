#' @title Leipzig Land Cover Task
#'
#' @name leipzig
#' @aliases mlr_tasks_leipzig
#'
#' @description
#' Point survey of land cover in Leipzig.
#' Includes Sentinel-2 spectral bands and NDVI.
#'
#' @source
#' Copernicus Sentinel Data (2021). Retrieved from Copernicus Open Access Hub and processed by European Space Agency.
#'
#' @docType data
#' @keywords data
#' @examples
#' if(requireNamespace("sf")) {
#' library(sf)
#' data("leipzig", package = "mlr3spatial")
#' print(leipzig)
#' }
NULL

load_task_leipzig = function(id = "leipzig") {
  vector = sf::read_sf(system.file("extdata", "leipzig_points.gpkg", package = "mlr3spatial"), stringsAsFactors = TRUE)
  task = as_task_classif(vector, id = id, target = "land_cover",  label = "Leipzig Land Cover")
  task$backend$hash = task$man = "mlr3::mlr_tasks_leipzig"
  task
}
