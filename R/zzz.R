#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @import mlr3
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

with_future = function(backend, expr, ...) {
  requireNamespace("future")
  oplan = force(future::plan(backend, ...))
  on.exit(future::plan(oplan), add = TRUE)
  force(expr)
}
