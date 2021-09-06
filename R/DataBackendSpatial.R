#' @title DataBackend for spatial objects
#'
#' @description
#' [mlr3::DataBackend] for spatial objects:
#' - [raster::brick]
#' - [stars::st_as_stars]
#' - [terra::SpatRaster]
#'
#' The DataBackend can be constructed using the spatial objects listed above.
#' Internally {terra} is used for processing operations.
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
#'  There are two different ways the reading of values is performed internally:
#' * "Block mode" reads complete rows of the raster file and subsets the
#' requested cells. This mode is faster than "cell mode" if the complete raster
#' file is iterated over.
#'
#' * "Cell mode" reads individual cells. This is faster than "block mode" if
#' only a few cells are sampled.
#'
#' "Block mode" is activated if `$data(rows)` is used with a increasing integer
#' sequence e.g. `200:300`. If only a single cell is requested, "cell mode" is
#' used.
#'
#' @importFrom terra writeRaster writeStart writeStop rast cats sources
#'   intersect readStart readStop rowColFromCell readValues head unique ncell
#'   nlyr ncol
#' @importFrom methods as
#' @export
DataBackendSpatial = R6Class("DataBackendSpatial",
  inherit = DataBackend, cloneable = FALSE,
  public = list(

    #' @description
    #'
    #' Creates a backend for a `SpatRaster`.
    #'
    #' @template param-data
    #'
    initialize = function(data) {

      if (inherits(data, "stars")) {
        # we need to go stars -> raster -> terra
        data = terra::rast(as(data, "Raster"))
        DataBackendSpatial$new(data)
      } else if (inherits(data, "Raster")) {
        data = terra::rast(data)
      }
      assert_class(data, "SpatRaster")
      # FIXME: use inMemory function
      # write raster to disk
      if (all(data@ptr$inMemory)) {
        filename = tempfile(fileext = ".tif")
        terra::writeRaster(data, filename = filename)
        data = terra::rast(filename)
      }
      private$.data = terra::sources(assert_class(data, "SpatRaster"))$source
      private$.categories = terra::cats(data)
      private$.layer_names = names(data)
      self$data_formats = "data.table"
    },

    #' @description
    #' Returns a slice of the raster in the specified format.
    #' Currently, the only supported formats is `"data.table"`.
    #'
    #' The rows must be addressed as vector of cells indices, columns must be
    #' referred to via layer names. Queries for rows with no matching row id and
    #' queries for columns with no matching column name are silently ignored.
    #'
    #' Rows are guaranteed to be returned in the same order as `rows`, columns
    #' may be returned in an arbitrary order. Duplicated row ids result in
    #' duplicated rows, duplicated column names lead to an exception.
    #'
    #' @param data_format (`character(1)`)\cr
    #'   Desired data format. Currently only `"data.table"` supported.
    data = function(rows, cols, data_format = "data.table") {
      stack = self$stack
      rows = assert_integerish(rows, coerce = TRUE)
      assert_names(cols, type = "unique")
      assert_choice(data_format, self$data_formats)
      cols = terra::intersect(cols, names(self$stack))

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
      as.data.table(terra::head(self$stack, n))
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
      assert_names(cols, type = "unique")
      if (length(cols) == 0) {
        return(NULL)
      }
      cols = intersect(cols, self$colnames)

      if (is.null(rows)) {
        stack = terra::subset(self$stack, cols)
        if (all(terra::is.factor(stack))) {
          # fastest
          res = as.list(map_dtc(terra::cats(stack), function(layer) as.data.table(layer)[, 2]))
        } else {
          # fast
          res = terra::unique(stack, incomparables = TRUE)
          set_names(res, names(stack))
        }
      } else {
        # slow
        data = self$data(rows, cols)
        res = lapply(data, unique)
      }
      if (na_rm) res = map(res, function(values) values[!is.na(values)])
      res
    },

    #' @description
    #' Returns the number of missing values per column in the specified slice
    #' of data. Non-existing rows and columns are silently ignored.
    #'
    #' @return Total of missing values per column (named `numeric()`).
    missings = function(rows, cols) {
      cols = terra::intersect(cols, self$colnames)

      stack = self$stack
      res = terra::freq(stack, value = NA)
      set_names(res[, "count"], names(stack))[cols]
    }
  ),

  active = list(
    #' @field rownames (`integer()`)\cr
    #' Returns vector of all distinct row identifiers, i.e. the contents of the primary key column.
    rownames = function(rhs) {
      assert_ro_binding(rhs)
      1:terra::ncell(self$stack)
    },

    #' @field colnames (`character()`)\cr
    #' Returns vector of all column names.
    colnames = function(rhs) {
      assert_ro_binding(rhs)
      names(self$stack)
    },

    #' @field nrow (`integer(1)`)\cr
    #' Number of rows (observations).
    nrow = function(rhs) {
      assert_ro_binding(rhs)
      terra::ncell(self$stack)
    },

    #' @field ncol (`integer(1)`)\cr
    #' Number of columns (variables).
    ncol = function(rhs) {
      assert_ro_binding(rhs)
      terra::nlyr(self$stack) + 1
    },

    #' @field stack (`SpatRaster`)\cr
    #' Raster stack.
    stack = function(rhs) {
      assert_ro_binding(rhs)
      stack = terra::rast(private$.data)
      names(stack) = private$.layer_names
      imap(private$.categories, function(category, n) {
        if (nrow(category) > 0) {
          terra::setCats(stack, layer = n, value = category)
        }
      })
      stack
    }
  ),

  private = list(
    .calculate_hash = function() {
      mlr3misc::calculate_hash(self$compact_seq, self$stack)
    },

    .categories = NULL,
    .layer_names = NULL
  )
)

#' @title Coerce to DataBackendSpatial
#'
#' @description
#' Wraps a [DataBackend] around spatial objects.
#' Currently this is only a synonym for `DataBackenSpatial$new()` and does not
#' support coercing from other backends.
#'
#' @template param-data
#' @template param-primary-key
#' @param ... (`any`)\cr
#'   Not used.
#'
#' @return [DataBackend].
#' @rdname as_data_backend
#'
#' @export
as_data_backend.stars = function(data, primary_key = NULL, ...) { # nolint
  # we need to go stars -> raster -> terra
  data = terra::rast(as(data, "Raster"))
  DataBackendSpatial$new(data)
}
#' @export
as_data_backend.SpatRaster = function(data, primary_key = NULL, ...) { # nolint
  DataBackendSpatial$new(data)
}
#' @export
as_data_backend.RasterBrick = function(data, primary_key = NULL, ...) { # nolint
  data = terra::rast(data)
  DataBackendSpatial$new(data)
}
#' @export
as_data_backend.Raster = function(data, primary_key = NULL, ...) { # nolint
  data = terra::rast(data)
  DataBackendSpatial$new(data)
}
