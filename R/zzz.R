#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @import mlr3
#' @import sf
#' @importFrom R6 R6Class is.R6
#' @importFrom stats complete.cases
#' @importFrom utils getFromNamespace data
#'
#' @section Learn mlr3:
#' * Book on mlr3: \url{https://mlr3book.mlr-org.com}
#' * Use cases and examples gallery: \url{https://mlr3gallery.mlr-org.com}
#' * Cheat Sheets: \url{https://github.com/mlr-org/mlr3cheatsheets}
#'
#' @section mlr3 extensions:
#' * Preprocessing and machine learning pipelines: \CRANpkg{mlr3pipelines}
#' * Analysis of benchmark experiments: \CRANpkg{mlr3benchmark}
#' * More classification and regression tasks: \CRANpkg{mlr3data}
#' * Connector to [OpenML](https://www.openml.org): \CRANpkg{mlr3oml}
#' * Solid selection of good classification and regression learners: \CRANpkg{mlr3learners}
#' * Even more learners: \url{https://github.com/mlr-org/mlr3extralearners}
#' * Tuning of hyperparameters: \CRANpkg{mlr3tuning}
#' * Hyperband tuner: \CRANpkg{mlr3hyperband}
#' * Visualizations for many \pkg{mlr3} objects: \CRANpkg{mlr3viz}
#' * Survival analysis and probabilistic regression: \CRANpkg{mlr3proba}
#' * Cluster analysis: \CRANpkg{mlr3cluster}
#' * Feature selection filters: \CRANpkg{mlr3filters}
#' * Feature selection wrappers: \CRANpkg{mlr3fselect}
#' * Interface to real (out-of-memory) data bases: \CRANpkg{mlr3db}
#' * Performance measures as plain functions: \CRANpkg{mlr3measures}
#'
#' @section Suggested packages:
#' * Parallelization framework: \CRANpkg{future}
#' * Progress bars: \CRANpkg{progressr}
#' * Encapsulated evaluation: \CRANpkg{evaluate}, \CRANpkg{callr} (external process)
#'
#' @section Package Options:
#' * `"mlr3.debug"`: If set to `TRUE`, parallelization via \CRANpkg{future} is
#' disabled to simplify debugging and provide more concise tracebacks. Note that
#' results computed with debug mode enabled use a different seeding mechanism
#' and are not reproducible.
#' * `"mlr3.allow_utf8_names"`: If set to `TRUE`, checks on the feature names
#' are relaxed, allowing non-ascii characters in column names. This is an
#' experimental and temporal option to pave the way for text analysis, and will
#' likely be removed in a future version of the package. analysis.
#'
#' @references
#' `r tools::toRd(citation("mlr3spatial"))`
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start

  # reflections
  x = getFromNamespace("mlr_reflections", ns = "mlr3")

  x$task_types = x$task_types[!c("regr_st", "classif_st")]
  x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
    ~type,        ~package,      ~task,            ~learner,             ~prediction,         ~prediction_data,         ~measure,
    "regr_st",    "mlr3spatial", "TaskRegrST",     "LearnerRegr",        "PredictionRegr",    "PredictionDataRegr",     "MeasureRegr",
    "classif_st", "mlr3spatial", "TaskClassifST",  "LearnerClassif",     "PredictionClassif", "PredictionDataClassif",  "MeasureClassif"
  )), "type")

  x$task_col_roles$classif_st = c(x$task_col_roles$classif, c("coordinate", "space", "time"))
  x$task_col_roles$regr_st = c(x$task_col_roles$regr, c("coordinate", "space", "time"))
  x$task_col_roles$unsupervised = x$task_col_roles$regr

  x$task_properties$classif_st = x$task_properties$classif
  x$task_properties$regr_st = x$task_properties$regr

  x$default_measures$classif_st = "classif.ce"
  x$default_measures$regr_st = "regr.mse"

  # task
  x = getFromNamespace("mlr_tasks", ns = "mlr3")
  x$add("leipzig", load_task_leipzig)

  # setup logger
  assign("lg", lgr::get_logger("mlr3"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end


.onUnload = function(libpaths) { # nolint
  mlr3::mlr_tasks$remove("leipzig")
}

leanify_package() # nocov
