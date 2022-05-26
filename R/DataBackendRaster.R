#' @title DataBackend for raster objects
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
    #' @field compact_seq `logical(1)`\cr
    #' If `TRUE`, row ids are a natural sequence from 1 to `nrow(data)` (determined internally).
    #' In this case, row lookup uses faster positional indices instead of equi joins.
    compact_seq = FALSE,

    #' @description
    #'
    #' Creates a backend for a raster objects.
    #'
    #' @template param-data
    #' @template param-train-task
    #'
    initialize = function(data, train_task = NULL) {
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

      # cache default active fields
      private$.rownames = 1:terra::ncell(data)
      private$.colnames = names(data)
      private$.nrow = terra::ncell(data)
      private$.ncol = terra::nlyr(data)

      # train task
      if (!is.null(train_task)) {
        assert_task(train_task)
        if (train_task$target_names %in% private$.colnames) stopf("Target of %s is already a layer in data.", format(train_task))
        private$.colnames = c(private$.colnames, train_task$target_names)
        private$.ncol = private$.ncol + 1
        private$.response = switch(train_task$task_type,
          "classif" = factor(NA_character_, levels = train_task$levels()[[train_task$target_names]]),
          "regr" = NA_real_
        )
        private$.target_names = train_task$target_names
      }
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
      cols_stack = intersect(cols, private$.layer_names)

      # data from stack
      res = if (!length(cols_stack)) {
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
        res[cells[1, 2]:(cells[1, 2] + length(rows) - 1), cols_stack, with = FALSE]
      } else {
        # cell read (e.g. c(1, 3, 5, 6, 10))
        cells = terra::rowColFromCell(stack, rows)
        as.data.table(terra::extract(stack, rows))[, cols_stack, with = FALSE]
      }

      # response from train task
      if (private$.target_names %in% cols) set(res, j = private$.target_names, value = private$.response)

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
      cols_stack = intersect(cols, private$.layer_names)
      rows = rows %??% seq(self$nrow)

      res = if (!length(cols_stack)) {
        named_list()
      } else if (test_integer(rows, sorted = TRUE, unique = TRUE, len = self$nrow)) {
        # fast
        stack = terra::subset(self$stack, cols_stack)
        set_names(map(cols_stack, function(layer) {
          if (terra::is.factor(stack[layer])) {
            terra::cats(stack[layer])[[1]][, 2]
          } else {
            terra::unique(stack["x_1"])[[1]]
          }
        }), cols_stack)
      } else {
        # slow
        data = self$data(rows, cols_stack)
        res = map(data, unique)
        map_if(res, is.factor, as.character)
      }

      if (private$.target_names %in% cols) {
        response = if (is.factor(private$.response)) levels(private$.response) else NA_real_
        res = c(res, set_names(list(response), private$.target_names))
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
      cols_stack = intersect(cols, private$.layer_names)

      res = if (!length(cols_stack)) {
        numeric(0)
      } else if (test_integer(rows, sorted = TRUE, unique = TRUE, len = self$nrow)) {
        # fast
        stack = self$stack
        res = terra::freq(stack, value = NA)
        res = set_names(res[, "count"], private$.layer_names)[cols_stack]
        res
      } else {
        # slow
        data = self$data(rows, cols_stack)
        map_int(data, count_missing)
      }

      if (private$.target_names %in% cols) res = c(res, set_names(length(rows), private$.target_names))
      res
    }
  ),

  active = list(
    #' @field rownames (`integer()`)\cr
    #' Returns vector of all distinct row identifiers, i.e. the contents of the primary key column.
    rownames = function(rhs) {
      assert_ro_binding(rhs)
      private$.rownames
    },

    #' @field colnames (`character()`)\cr
    #' Returns vector of all column names.
    colnames = function(rhs) {
      assert_ro_binding(rhs)
      private$.colnames
    },

    #' @field nrow (`integer(1)`)\cr
    #' Number of rows (observations).
    nrow = function(rhs) {
      assert_ro_binding(rhs)
      private$.nrow
    },

    #' @field ncol (`integer(1)`)\cr
    #' Number of columns (variables).
    ncol = function(rhs) {
      assert_ro_binding(rhs)
      private$.ncol
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
      stack
    }
  ),

  private = list(
    .calculate_hash = function() {
      mlr3misc::calculate_hash(self$compact_seq, self$stack)
    },
    # stack
    .categories = NULL,
    .layer_names = NULL,

    # cache
    .rownames = NULL,
    .colnames = NULL,
    .nrow = NULL,
    .ncol = NULL,

    # fake response
    .response = NULL,
    .target_names = character(1)
  )
)

#' @title Coerce to spatial DataBackend
#'
#' @description
#' Wraps a [DataBackend] around spatial objects.
#' Currently these S3 methods are only alternative ways for writing `DataBackendRaster$new()` or `DataBackendVector$new()`.
#' They do not support coercing from other backends yet.
#'
#' @template param-data
#' @template param-primary-key
#' @template param-train-task
#' @param ... (`any`)\cr
#'   Not used.
#'
#' @return [DataBackend].
#' @rdname as_data_backend
#'
#' @exportS3Method
#' @export as_data_backend.stars
as_data_backend.stars = function(data, primary_key = NULL, train_task = NULL, ...) { # nolint
  require_namespaces("stars")
  data = as(data, "SpatRaster")
  DataBackendRaster$new(data, train_task = train_task)
}
#' @export as_data_backend.SpatRaster
#' @exportS3Method
#' @rdname as_data_backend
as_data_backend.SpatRaster = function(data, primary_key = NULL, train_task = NULL, ...) { # nolint
  DataBackendRaster$new(data, train_task = train_task)
}
#' @export as_data_backend.RasterBrick
#' @exportS3Method
#' @rdname as_data_backend
as_data_backend.RasterBrick = function(data, primary_key = NULL, train_task = NULL, ...) { # nolint
  data = terra::rast(data)
  DataBackendRaster$new(data, train_task = train_task)
}
#' @export as_data_backend.Raster
#' @exportS3Method
#' @rdname as_data_backend
as_data_backend.Raster = function(data, primary_key = NULL, train_task = NULL, ...) { # nolint
  data = terra::rast(data)
  DataBackendRaster$new(data, train_task = train_task)
}
