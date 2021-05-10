#' @import data.table
#' @import checkmate
#' @importFrom mlr3learners LearnerClassifSVM
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
