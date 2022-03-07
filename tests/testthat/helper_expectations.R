expect_backend = function(b) {
  checkmate::expect_r6(b, cloneable = FALSE,
    public = c("nrow", "ncol", "colnames", "rownames", "head", "data", "hash"),
    private = c(".data", ".hash", ".calculate_hash"))
  checkmate::expect_subset(b$data_formats, mlr3::mlr_reflections$data_formats, empty.ok = FALSE)
  testthat::expect_output(print(b), "^<DataBackend")

  n = checkmate::expect_count(b$nrow)
  p = checkmate::expect_count(b$ncol)
  cn = checkmate::expect_character(b$colnames, any.missing = FALSE, len = p, unique = TRUE)
  rn = checkmate::expect_integerish(b$rownames, any.missing = FALSE, len = n, unique = TRUE)
  rn1 = head(rn, 1L)
  pk = b$primary_key

  x = b$data(rows = rn, cols = pk, data_format = "data.table")
  checkmate::expect_data_table(x, ncols = 1L, nrows = n, col.names = "unique")
  x = x[[1L]]
  checkmate::expect_integerish(x, len = n, unique = TRUE)

  x = b$data(rows = rn, cols = setdiff(cn, pk)[1L], data_format = "data.table")
  checkmate::expect_data_table(x, ncols = 1L, nrows = n, col.names = "unique")
  x = x[[1L]]
  checkmate::expect_atomic_vector(x, len = n)

  # extra cols are ignored
  x = b$data(rows = rn1, cols = c(cn[1L], "_not_existing_"), data_format = "data.table")
  checkmate::expect_data_table(x, nrows = length(rn1), ncols = 1L)

  # zero cols matching
  x = b$data(rows = rn1, cols = "_not_existing_", data_format = "data.table")
  checkmate::expect_data_table(x, nrows = 0L, ncols = 0L)

  # extra rows are ignored
  query_rows = c(rn1, if (is.integer(rn)) -1L else "_not_existing_")
  x = b$data(query_rows, cols = cn[1L], data_format = "data.table")
  checkmate::expect_data_table(x, nrows = length(rn1), ncols = 1L)

  # zero rows matching
  query_rows = if (is.integer(rn)) -1L else "_not_existing_"
  x = b$data(rows = query_rows, cols = cn[1L], data_format = "data.table")
  checkmate::expect_data_table(x, nrows = 0L, ncols = 1L)

  # rows are duplicated
  x = b$data(rows = rep(rn1, 2L), cols = b$colnames, data_format = "data.table")
  checkmate::expect_data_table(x, nrows = 2L * length(rn1), ncols = p)

  # cols are returned in the right order
  j = rev(cn)
  x = b$data(rows = rn1, cols = j, data_format = "data.table")
  testthat::expect_equal(j, colnames(x))

  # rows are returned in the right order
  i = sample(rn, min(n, 10L))
  x = b$data(rows = i, cols = b$primary_key, data_format = "data.table")
  testthat::expect_equal(i, x[[1L]])

  # duplicated cols raise exception
  testthat::expect_error(b$data(rows = rn1, cols = rep(cn[1L], 2L), data_format = "data.table"), "unique")

  # $head()
  checkmate::expect_data_table(b$head(.Machine$integer.max), nrows = n, ncols = p)
  checkmate::expect_data_table(b$head(0L), nrows = 0, ncols = p)
  checkmate::expect_data_table(b$head(2L), nrows = min(2L, b$nrow), ncols = p)

  # $distinct()
  d = b$distinct(rn, b$primary_key)[[1L]]
  checkmate::expect_integerish(d, any.missing = FALSE, len = n, unique = TRUE)
  checkmate::expect_list(b$distinct(rn, "_not_existing_"), len = 0L, names = "named")
  d = b$distinct(rn, c("_not_existing_", rev(cn), "_also_not_existing_"))
  checkmate::expect_list(d, names = "unique")
  testthat::expect_equal(names(d), rev(cn))

  d = b$distinct(rn1, cn)
  checkmate::expect_list(d, len = length(cn), names = "unique", any.missing = FALSE)
  testthat::expect_true(all(lengths(d) <= 1L)) # NA -> 0 zero length

  ## missings are handled by distinct?
  d = b$distinct(rn, cn, na_rm = TRUE)
  checkmate::qexpectr(d, "V")

  d = b$distinct(rn, cn, na_rm = FALSE)
  m = b$missings(rn, cn)
  testthat::expect_equal(vapply(d, checkmate::anyMissing, FUN.VALUE = logical(1)), m > 0L)

  # $missings()
  x = b$missings(b$rownames, b$colnames)
  checkmate::expect_integerish(x, lower = 0L, upper = b$nrow, any.missing = FALSE)
  checkmate::expect_names(names(x), permutation.of = b$colnames)
  checkmate::expect_integerish(b$missings(b$rownames, "_not_existing_"), len = 0L, names = "named")
  checkmate::expect_integerish(b$missings(b$rownames[0L], b$colnames), len = b$ncol, names = "unique")
  checkmate::expect_integerish(b$missings(b$rownames[0L], "_not_existing_"), len = 0L, names = "unique")

  # $hash
  checkmate::expect_string(b$hash)
}
