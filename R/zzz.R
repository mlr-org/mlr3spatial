#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @import terra
#' @importFrom R6 R6Class is.R6
#' @references
#' \cite{mlr3spatial}{pkg::citation}
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start

  # setup logger
  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end
