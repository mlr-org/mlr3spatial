#' @title Spatiotemporal Classification Task
#'
#' @description
#' This task specializes [TaskClassif] for spatiotemporal classification problems.
#'
#' A spatial example task is available via `tsk("ecuador")`.
#'
#' The coordinate reference system passed during initialization must match the one which was used during data creation, otherwise offsets of multiple meters may occur.
#' By default, coordinates are not used as features.
#' This can be changed by setting `coords_as_features = TRUE`.
#'
#' @template param_id
#' @template param_backend
#' @template param_target
#' @template param_positive
#' @template param_label
#' @template param_coords_as_features
#' @template param_crs
#' @template param_coordinate_names
#' @template param_extra_args
#'
#' @export
TaskClassifST = R6::R6Class("TaskClassifST",
  inherit = TaskClassif,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' The function [as_task_classif_st()] provides an alternative way to construct classification tasks.
    initialize = function(id, backend, target, positive = NULL, label = NA_character_, coordinate_names, crs = NA_character_, coords_as_features = FALSE, extra_args = list()) {
      super$initialize(id = id, backend = backend, target = target, positive = positive, extra_args = extra_args)
      self$crs = crs
      self$coordinate_names = coordinate_names
      walk(coordinate_names, function(x) assert_numeric(self$backend$head(1)[[x]], .var.name = x))

      # adjust classif task
      self$task_type = "classif_st"
      new_col_roles = named_list(setdiff(mlr_reflections$task_col_roles[["classif_st"]], names(private$.col_roles)), character(0))
      private$.col_roles = insert_named(private$.col_roles, new_col_roles)

      # add coordinates as features
      self$coords_as_features = assert_flag(coords_as_features)
    },

    #' @description
    #' Returns coordinates of observations.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Vector of rows indices as subset of `task$row_ids`.
    #'
    #' @return [data.table::data.table()]
    coordinates = function(row_ids = NULL) {
      if (is.null(row_ids)) row_ids = self$row_ids
      self$backend$data(rows = row_ids, cols = self$coordinate_names)
    },

    #' @description
    #' Print the task.
    #'
    #' @param ... Arguments passed to the `$print()` method of the superclass.
    print = function(...) {
      super$print(...)
      catn("* Coordinates:")
      print(self$coordinates(), nrows = 10)
    }
  ),

  active = list(

    #' @field crs (`character(1)`)\cr
    #'   Returns coordinate reference system of task.
    crs = function(rhs) {
      if (missing(rhs)) {
        return(self$extra_args$crs)
      }
      self$extra_args$crs = assert_string(rhs, na.ok = TRUE)
    },

    #' @field coordinate_names (`character()`)\cr
    #'   Returns coordinate names.
    coordinate_names = function(rhs) {
      if (missing(rhs)) {
        return(self$extra_args$coordinate_names)
      }
      self$extra_args$coordinate_names = assert_character(rhs, len = 2, all.missing = FALSE, any.missing = FALSE)
    },

    #' @field coords_as_features (`logical(1)`)\cr
    #'   If `TRUE`, coordinates are used as features.
    coords_as_features = function(rhs) {
      if (missing(rhs)) {
        return(self$extra_args$coords_as_features)
      }

      self$extra_args$coords_as_features = assert_flag(rhs)
      if (rhs) {
        self$set_col_roles(self$coordinate_names, add_to = "coordinate")
      } else {
        self$set_col_roles(self$coordinate_names, roles = "coordinate")
      }
    }
  )
)
