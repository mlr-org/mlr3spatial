#' @title DataBackend for Raster Objects
#'
#' @description
#' [mlr3::DataBackend] for [terra::SpatRaster] raster objects.
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
#' @importFrom utils tail
#' @importFrom methods as
#' @export
DataBackendRaster = R6Class("DataBackendRaster",
  inherit = DataBackend, cloneable = FALSE,
  public = list(

    #' @description
    #'
    #' Creates a backend for a raster objects.
    #'
    #' @template param-data
    #'
    initialize = function(data) {
      assert_class(data, "SpatRaster")

      # write raster to disk
      sources = map_chr(names(data), function(layer) {
        if (data[layer]@ptr$inMemory) {
          filename = tempfile(fileext = ".tif")
          terra::writeRaster(data[layer], filename = filename)
        } else {
          filename = terra::sources(data[layer])
        }
        filename
      })

      # stack
      private$.data = unique(sources) # nolint
      private$.categories = terra::cats(data)
      private$.layer_names = names(data)
      private$.crs = terra::crs(data)

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
      if (is.null(rows)) rows = numeric(0)
      rows = assert_integerish(rows, coerce = TRUE, null.ok = TRUE)
      assert_names(cols, type = "unique")
      assert_choice(data_format, self$data_formats)
      cols = intersect(cols, private$.layer_names)

      if (!length(cols)) {
        data.table()
      } else if (length(rows) && test_integer(rows, sorted = TRUE, unique = TRUE, len = max(rows[length(rows)] - rows[1] + 1, 0))) {
        # block read (e.g. c(1:10))
        terra::readStart(stack)
        on.exit(terra::readStop(stack))
        # determine rows to read
        cells = terra::rowColFromCell(stack, rows)
        row = cells[1, 1]
        nrows = cells[dim(cells)[1], 1] - cells[1, 1] + 1
        res = as.data.table(terra::readValues(stack, row = row, nrows = nrows, dataframe = TRUE))
        # subset cells and features
        res[cells[1, 2]:(cells[1, 2] + length(rows) - 1), cols, with = FALSE]
      } else {
        # cell read (e.g. c(1, 3, 5, 6, 10))
        as.data.table(terra::extract(stack, rows))[, cols, with = FALSE]
      }
    },

    #' @description
    #' Retrieve the first `n` rows.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of rows.
    #'
    #' @return [data.table::data.table()] of the first `n` rows.
    head = function(n = 6L) {
      res = as.data.table(terra::head(self$stack, n))
      if (length(private$.response)) set(res, j = private$.target_names, value = private$.response)
      res
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
      cols = intersect(cols, private$.layer_names)
      rows = rows %??% seq(self$nrow)

      res = if (!length(cols)) {
        named_list()
      } else if (test_integer(rows, sorted = TRUE, unique = TRUE, len = self$nrow)) {
        # fast
        stack = terra::subset(self$stack, cols)
        set_names(map(cols, function(layer) {
          if (terra::is.factor(stack[layer])) {
            terra::cats(stack[layer])[[1]][, 2]
          } else {
            terra::unique(stack["x_1"])[[1]]
          }
        }), cols)
      } else {
        # slow
        data = self$data(rows, cols)
        res = map(data, unique)
        map_if(res, is.factor, as.character)
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
      cols = intersect(cols, private$.layer_names)

      if (!length(cols)) {
        numeric(0)
      } else if (test_integer(rows, sorted = TRUE, unique = TRUE, len = self$nrow)) {
        # fast
        stack = self$stack
        res = terra::freq(stack, value = NA)
        res = set_names(res[, "count"], private$.layer_names)[cols]
        res
      } else {
        # slow
        data = self$data(rows, cols)
        map_int(data, count_missing)
      }
    },

    #' @description
    #' Returns the coordinates of `rows`.
    #' If `rows` is missing, all coordinates are returned.
    #'
    #' @return [data.table::data.table()] of coordinates of `rows`.
    coordinates = function(rows) {
      if (missing(rows)) {
        as.data.table(terra::crds(self$stack, df = TRUE))
      } else {
        as.data.table(terra::xyFromCell(rows))
      }
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
      terra::nlyr(self$stack)
    },

    #' @field stack (`SpatRaster`)\cr
    #' Raster stack.
    stack = function(rhs) {
      assert_ro_binding(rhs)
      stack = terra::rast(private$.data)
      iwalk(private$.categories, function(category, n) {
        if (!is.null(category)) {
          terra::set.cats(stack, layer = n, value = category)
        }
      })
      terra::set.names(stack, private$.layer_names)
      terra::crs(stack) = private$.crs
      stack
    }
  ),

  private = list(
    .calculate_hash = function() {
      mlr3misc::calculate_hash(self$stack)
    },
    .categories = NULL,
    .layer_names = NULL,
    .crs = NULL
  )
)

#' @title Coerce to spatial DataBackend
#'
#' @description
#' Wraps a [DataBackend] around spatial objects.
#' Currently these S3 methods are only alternative ways for writing `DataBackendRaster$new()`.
#' They do not support coercing from other backends yet.
#'
#' @template param-data
#' @template param-primary-key
#' @param ... (`any`)\cr
#'   Not used.
#'
#' @return [DataBackend].
#' @rdname as_data_backend
#'
#' @exportS3Method
#' @export as_data_backend.stars
as_data_backend.stars = function(data, primary_key = NULL, ...) { # nolint
  require_namespaces("stars")
  data = as(data, "SpatRaster")
  DataBackendRaster$new(data)
}

#' @export as_data_backend.SpatRaster
#' @exportS3Method
#' @rdname as_data_backend
as_data_backend.SpatRaster = function(data, primary_key = NULL, ...) { # nolint
  DataBackendRaster$new(data)
}

#' @export as_data_backend.RasterBrick
#' @exportS3Method
#' @rdname as_data_backend
as_data_backend.RasterBrick = function(data, primary_key = NULL, ...) { # nolint
  data = terra::rast(data)
  DataBackendRaster$new(data)
}

#' @export as_data_backend.RasterStack
#' @exportS3Method
#' @rdname as_data_backend
as_data_backend.RasterStack = function(data, primary_key = NULL, ...) { # nolint
  data = terra::rast(data)
  DataBackendRaster$new(data)
}
