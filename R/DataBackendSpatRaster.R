#' @title DataBackend for SpatRaster
#'
#' @description
#' A [mlr3::DataBackend] for `SpatRaster` (package \CRANpkg{terra}).
#'
#' @param rows `integer()`\cr
#'   Row indices. Row indices start with 1 in the upper left corner in the
#'   raster, increase from left to right and then from top to bottom. The last
#'   cell is in the bottom right corner and the row index equals the number of
#'   cells in the raster.
#' @param cols `character()`\cr
#'   Column names.
#'
#' @section Read mode:
#' * Block mode reads complete rows of the raster file and subsets the requested
#'   cells. Faster than cell mode if we iterate the whole raster file.
#'
#' * Cell mode reads individual cells. Faster than block mode if only a few
#'   cells are sampled.
#'
#' Block mode is activated if `$data(rows)` is called with a increasing integer
#' sequence e.g. `200:300`.
#' @examples
#' if (mlr3misc::require_namespaces("terra", quietly = TRUE)) {
#'   stack = demo_stack_spatraster(size = 5, layers = 5)
#'   backend = DataBackendSpatRaster$new(stack)
#' }
#' @export
DataBackendSpatRaster = R6::R6Class("DataBackendSpatRaster",
  inherit = mlr3::DataBackend, cloneable = FALSE,
  public = list(

    #' @field compact_seq `logical(1)`\cr
    #' If `TRUE`, row ids are a natural sequence from 1 to `nrow(data)` (determined internally).
    #' In this case, row lookup uses faster positional indices instead of equi joins.
    compact_seq = FALSE,

    #' @description
    #'
    #' Creates a backend for a `SpatRaster`.
    #'
    #' @param data (`SpatRaster`)\cr
    #'    A raster object.
    #'
    initialize = function(data) {
      private$.spatraster = terra::wrap(data)

      terra::readStart(data)
      on.exit(terra::readStop(data))
      values_dt = as.data.table(terra::readValues(data, dataframe = TRUE))

      row_ids = seq_row(values_dt)

      primary_key = "..row_id"
      # FIXME: I think we can do this better?
      values_dt = suppressWarnings(mlr3misc::insert_named(values_dt, list("..row_id" = row_ids)))

      super$initialize(setkeyv(values_dt, primary_key), primary_key, data_formats = "data.table")
      ii = match(primary_key, names(values_dt))
      if (is.na(ii)) {
        stopf("Primary key '%s' not in 'data'", primary_key)
      }

      private$.data = assert_class(values_dt, "data.table")
      self$data_formats = "data.table"
      private$.cache = set_names(replace(rep(NA, ncol(values_dt)), ii, FALSE), names(values_dt))
    },

    #' @description
    #' Returns a slice of the data.
    #' Calls [raster::rowColFromCell()] and [raster::getValues()] on the spatial
    #' object and converts it to a [data.table::data.table()].
    #'
    #' The rows must be addressed as vector of primary key values, columns must
    #' be referred to via column names. Queries for rows with no matching row id
    #' and queries for columns with no matching column name are silently
    #' ignored. Rows are guaranteed to be returned in the same order as `rows`,
    #' columns may be returned in an arbitrary order. Duplicated row ids result
    #' in duplicated rows, duplicated column names lead to an exception.
    #' @param data_format (`character(1)`)\cr
    #'  Desired data format, e.g. `"data.table"` or `"Matrix"`.
    data = function(rows, cols, data_format = "data.table") {
      rows = assert_integerish(rows, coerce = TRUE)
      assert_names(cols, type = "unique")
      assert_choice(data_format, self$data_formats)
      cols = intersect(cols, colnames(private$.data))

      if (self$compact_seq) {
        # https://github.com/Rdatatable/data.table/issues/3109
        rows = keep_in_bounds(rows, 1L, nrow(private$.data))
        data = private$.data[rows, cols, with = FALSE]
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
    #'
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

    #' @field stack (`integer(1)`)\cr
    #' Returns SpatRaster.
    stack = function(rhs) {
      assert_ro_binding(rhs)
      terra::rast(private$.spatraster)
    }
  ),

  private = list(
    .calculate_hash = function() {
      mlr3misc::calculate_hash(self$compact_seq, private$.data)
    },
    .data = NULL,
    .spatraster = NULL,
    .cache = NULL
  )
)
