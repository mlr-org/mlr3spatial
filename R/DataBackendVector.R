#' @title DataBackend for Vector Objects
#'
#' @description
#' [mlr3::DataBackend] for [sf::sf] vector objects.
#'
#' @export
DataBackendVector = R6::R6Class("DataBackendVector",
  inherit = mlr3::DataBackendDataTable,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param data (`sf`)\cr
    #'    A raster object.
    #' @param primary_key (`character(1)` | `integer()`)\cr
    #'   Name of the primary key column, or integer vector of row ids.
    initialize = function(data, primary_key) {
      assert_class(data, "sf")
      self$data_formats = "data.table"

      # store geometry
      sf_column = attr(data, "sf_column")
      private$.sfc = data[[sf_column]]

      # store data
      data[[sf_column]] = NULL
      attr(data, "sf_column") = NULL
      data = as.data.table(data)

      super$initialize(data, primary_key)
    }
  ),

  active = list(
    #' @field sfc ([sf::sfc])\cr
    #' Returns the sfc object.
    sfc = function(rhs) {
      assert_ro_binding(rhs)
      private$.sfc
    }
  ),

  private = list(
    .sfc = NULL
  )
)

#' @param keep_rownames (`logical(1)` | `character(1)`)\cr
#'   If `TRUE` or a single string, keeps the row names of `data` as a new column.
#'   The column is named like the provided string, defaulting to `"..rownames"` for `keep_rownames == TRUE`.
#'   Note that the created column will be used as a regular feature by the task unless you manually change the column role.
#'   Also see [data.table::as.data.table()].
#' @rdname as_data_backend
#' @exportS3Method
#' @export as_data_backend.sf
as_data_backend.sf = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  assert_class(data, "sf")
  assert_data_frame(data, min.cols = 1L, col.names = "unique")

  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
    data[[keep_rownames]] = rownames(data)
  }

  compact_seq = FALSE

  if (is.character(primary_key)) {
    assert_string(primary_key)
    assert_choice(primary_key, colnames(data))
    assert_integer(data[[primary_key]], any.missing = FALSE, unique = TRUE)
  } else {
    if (is.null(primary_key)) {
      row_ids = seq_row(data)
      compact_seq = TRUE
    } else if (is.integer(primary_key)) {
      row_ids = assert_integer(primary_key, len = nrow(data), any.missing = FALSE, unique = TRUE)
    } else {
      stopf("Argument 'primary_key' must be NULL, a column name or a vector of ids")
    }

    primary_key = "..row_id"
    data[[primary_key]] = row_ids
  }

  b = DataBackendVector$new(data, primary_key)
  b$compact_seq = compact_seq

  return(b)
}
