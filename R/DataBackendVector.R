#' @title DataBackend for vector objects
#'
#' @description
#' A [mlr3::DataBackend] for vector objects (only package \CRANpkg{sf} is
#' supported).
#'
#' @param rows `integer()`\cr
#'   Row indices.
#' @param cols `character()`\cr
#'   Column names.
#'
#' @export
DataBackendVector = R6::R6Class("DataBackendVector",
  inherit = mlr3::DataBackend, cloneable = FALSE,
  public = list(
    #' @field compact_seq `logical(1)`\cr
    #' If `TRUE`, row ids are a natural sequence from 1 to `nrow(data)` (determined internally).
    #' In this case, row lookup uses faster positional indices instead of equi joins.
    compact_seq = FALSE,

    #' @description
    #'
    #' Creates a backend for spatial vector objects.
    #'
    #' @param data (`sf`)\cr
    #'    A raster object.
    #' @param primary_key (`character(1)` | `integer()`)\cr
    #'   Name of the primary key column, or integer vector of row ids.
    initialize = function(data, primary_key = NULL) {
      assert_class(data, "sf")
      sf_column = attr(data, "sf_column")
      private$.geometry = data[[sf_column]]
      self$data_formats = "data.table"

      data[[sf_column]] = NULL
      attr(data, "sf_column") = NULL
      data = as.data.table(data)

      if (is.character(primary_key)) { # nocov start
        assert_string(primary_key)
        assert_choice(primary_key, colnames(data))
        assert_integer(data[[primary_key]], any.missing = FALSE, unique = TRUE) # nocov end
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
        data = insert_named(data, list("..row_id" = row_ids))
      }

      assert_data_table(data, col.names = "unique")
      super$initialize(setkeyv(data, primary_key), primary_key, data_formats = "data.table")
      ii = match(primary_key, names(data))
      if (is.na(ii)) {
        stopf("Primary key '%s' not in 'data'", primary_key) # nocov
      }
      private$.cache = set_names(replace(rep(NA, ncol(data)), ii, FALSE), names(data))
    },

    #' @description
    #' Returns a slice of the data in the specified format.
    #' Currently, the only supported formats are `"data.table"` and `"Matrix"`.
    #' The rows must be addressed as vector of primary key values, columns
    #' must be referred to via column names.
    #' Queries for rows with no matching row id and queries for columns with
    #' no matching column name are silently ignored.
    #' Rows are guaranteed to be returned in the same order as `rows`, columns
    #' may be returned in an arbitrary order.
    #' Duplicated row ids result in duplicated rows, duplicated column names
    #' lead to an exception.
    #' @param data_format (`character(1)`)\cr
    #'  Desired data format, e.g. `"data.table"` or `"Matrix"`.
    data = function(rows, cols, data_format = "data.table") {
      rows = assert_integerish(rows, coerce = TRUE)
      assert_names(cols, type = "unique")
      assert_choice(data_format, self$data_formats)
      cols = intersect(cols, colnames(private$.data))

      if (self$compact_seq) { # nocov start
        # https://github.com/Rdatatable/data.table/issues/3109
        rows = keep_in_bounds(rows, 1L, nrow(private$.data))
        data = private$.data[rows, cols, with = FALSE] # nocov end
      } else {
        data = private$.data[list(rows), cols, with = FALSE, nomatch = NULL, on = self$primary_key]
      }
      return(data)
    },

    #' @description
    #' Retrieve the first `n` rows.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of rows.
    #'
    #' @return [data.table::data.table()] of the first `n` rows.
    head = function(n = 6L) {
      utils::head(private$.data, n)
    },

    #' @description
    #' Returns a named list of vectors of distinct values for each column
    #' specified. If `na_rm` is `TRUE`, missing values are removed from the
    #' returned vectors of distinct values. Non-existing rows and columns are
    #' silently ignored.
    #' @param na_rm `logical(1)`\cr
    #'   Whether to remove NAs or not.
    #'
    #' @return Named `list()` of distinct values.
    distinct = function(rows, cols, na_rm = TRUE) {
      cols = intersect(cols, colnames(private$.data))
      if (is.null(rows)) {
        set_names(lapply(cols, function(x) distinct_values(private$.data[[x]], drop = FALSE, na_rm = na_rm)), cols)
      } else {
        lapply(self$data(rows, cols), distinct_values, drop = TRUE, na_rm = na_rm)
      }
    },

    #' @description
    #' Returns the number of missing values per column in the specified slice
    #' of data. Non-existing rows and columns are silently ignored.
    #'
    #' @return Total of missing values per column (named `numeric()`).
    missings = function(rows, cols) {
      missind = private$.cache
      missind = missind[reorder_vector(names(missind), cols)]

      # update cache
      ii = which(is.na(missind))
      if (length(ii)) {
        missind[ii] = map_lgl(private$.data[, names(missind[ii]), with = FALSE], anyMissing)
        private$.cache = insert_named(private$.cache, missind[ii])
      }

      # query required columns
      query_cols = which(missind)
      insert_named(
        named_vector(names(missind), 0L),
        map_int(self$data(rows, names(query_cols)), count_missing)
      )

    }
  ),

  active = list(
    #' @field rownames (`integer()`)\cr
    #' Returns vector of all distinct row identifiers, i.e. the contents of the primary key column.
    rownames = function() {
      private$.data[[self$primary_key]]
    },

    #' @field colnames (`character()`)\cr
    #' Returns vector of all column names, including the primary key column.
    colnames = function() {
      colnames(private$.data)
    },

    #' @field nrow (`integer(1)`)\cr
    #' Number of rows (observations).
    nrow = function() {
      nrow(private$.data)
    },

    #' @field ncol (`integer(1)`)\cr
    #' Number of columns (variables), including the primary key column.
    ncol = function() {
      ncol(private$.data)
    },

    #' @field geometry (`integer(1)`)\cr
    #' Returns the sf geometry.
    geometry = function(rhs) {
      assert_ro_binding(rhs)
      private$.geometry
    }
  ),

  private = list(
    .calculate_hash = function() {
      mlr3misc::calculate_hash(self$compact_seq, private$.data)
    },
    .data = NULL,
    .cache = NULL,
    .geometry = NULL
  )
)

#' @rdname as_data_backend
#' @exportS3Method
#' @export as_data_backend.sf
as_data_backend.sf = function(data, primary_key = NULL, ...) { # nolint
  b = DataBackendVector$new(data, primary_key = primary_key)
  b$compact_seq = FALSE
  return(b)
}
