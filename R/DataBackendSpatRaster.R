#' @title DataBackend for SpatRaster
#'
#' @description
#' A [mlr3::DataBackend] for `SpatRaster` (\CRANpkg{terra}).
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
#' @importFrom terra readStart readStop rowColFromCell readValues head unique
#' @export
DataBackendSpatRaster = R6Class("DataBackendSpatRaster",
  inherit = mlr3::DataBackend, cloneable = FALSE,
  public = list(

    #' @description
    #'
    #' Creates a backend for a `SpatRaster`.
    #'
    #' @param data (`SpatRaster`)\cr
    #'    A raster object.
    #'
    initialize = function(data) {
      private$.data = assert_class(data, "SpatRaster")
      self$data_formats = "data.table"
    },

    #' @description
    #' Returns a slice of the data.
    #' Calls [dplyr::filter()] and [dplyr::select()] on the table and converts
    #' it to a [data.table::data.table()].
    #'
    #' The rows must be addressed as vector of primary key values, columns must be
    #' referred to via column names.
    #' Queries for rows with no matching row id and queries for columns with no matching
    #' column name are silently ignored.
    #' Rows are guaranteed to be returned in the same order as `rows`, columns
    #' may be returned in an arbitrary order.
    #' Duplicated row ids result in duplicated rows, duplicated column names lead to an exception.
    data = function(rows, cols, data_format = "data.table") {
      stack = private$.data

      if (isTRUE(all.equal(rows, rows[1]:rows[length(rows)]))) {
        # block read
        terra::readStart(stack)
        on.exit(terra::readStop(stack))
        # determine rows to read
        cells = terra::rowColFromCell(stack, rows)
        row = cells[1, 1]
        nrows = cells[dim(cells)[1], 1] - cells[1, 1] + 1
        res = as.data.table(terra::readValues(stack, row = row, nrows = nrows, dataframe = TRUE))
        # subset cells and features
        res = res[cells[1, 2]:(cells[1, 2] + length(rows) - 1), cols, with = FALSE]

      } else {
        # cell read
        cells = terra::rowColFromCell(stack, rows)
        res = rbindlist(apply(cells, 1, function(x) stack[x[1], x[2]][cols]))
      }
      res
    },

    #' @description
    #' Retrieve the first `n` rows.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of rows.
    #'
    #' @return [data.table::data.table()] of the first `n` rows.
    head = function(n = 6L) {
      as.data.table(terra::head(private$.data, n))
    },

    #' @description
    #' Returns a named list of vectors of distinct values for each column
    #' specified. If `na_rm` is `TRUE`, missing values are removed from the
    #' returned vectors of distinct values. Non-existing rows and columns are
    #' silently ignored.
    #'
    #' @return Named `list()` of distinct values.
    distinct = function(rows, cols, na_rm = TRUE) {
      assert_names(cols, type = "unique")
      cols = intersect(cols, self$colnames)

      if (is.null(rows)) {
        stack = terra::subset(private$.data, cols)
        if (all(terra::is.factor(stack))) {
          # fastest
          res = as.list(map_dtc(cats(stack), function(layer) as.data.table(layer)[, 2]))
        } else {
          # fast
          # bug: terra does not respect categorical raster layers
          terra::unique(stack, incomparables = TRUE)
          set_names(res, names(stack))
        }
      } else {
        # slow
        data = self$data(rows, cols)
        lapply(data, unique)
      }
    },

    #' @description
    #' Returns the number of missing values per column in the specified slice
    #' of data. Non-existing rows and columns are silently ignored.
    #'
    #' @return Total of missing values per column (named `numeric()`).
    missings = function(row, cols) {
      set_names(rep(0, self$ncol), self$colnames)
    }
  ),

  active = list(
    #' @field rownames (`integer()`)\cr
    #' Returns vector of all distinct row identifiers, i.e. the contents of the primary key column.
    rownames = function(rhs) {
      assert_ro_binding(rhs)
      1:ncell(private$.data)
    },

    #' @field colnames (`character()`)\cr
    #' Returns vector of all column names.
    colnames = function(rhs) {
      assert_ro_binding(rhs)
      names(private$.data)
    },

    #' @field nrow (`integer(1)`)\cr
    #' Number of rows (observations).
    nrow = function(rhs) {
      assert_ro_binding(rhs)
      ncell(private$.data)
    },

    #' @field ncol (`integer(1)`)\cr
    #' Number of columns (variables).
    ncol = function(rhs) {
      assert_ro_binding(rhs)
      terra::nlyr(private$.data) + 1
    },

    stack = function(rhs) {
      assert_ro_binding(rhs)
      private$.data
    }
  ),

  private = list(
    .calculate_hash = function() {
      mlr3misc::calculate_hash(self$compact_seq, private$.data)
    }
  )
)
