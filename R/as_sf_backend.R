#' @title Convert to sf backend
#' @description
#' Converts to a [DataBackendSF].
#' @inheritParams mlr3::as_data_backend
#' @inherit mlr3::as_data_backend description title
#' @rdname as_data_backend
#' @return [DataBackend].
#' @export
as_sf_backend = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  UseMethod("as_sf_backend")
}

#' @export
as_sf_backend.sf = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  assert_data_frame(data, min.cols = 1L, col.names = "unique")
  assert_class(data, "sf")
  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  b = DataBackendSF$new(data, primary_key)
  b$compact_seq = FALSE

  return(b)
}
