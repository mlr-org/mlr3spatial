#' @title Convert to a Spatiotemporal Classification Task
#'
#' @description
#' Convert object to a [TaskClassifST].
#' This is a S3 generic, specialized for at least the following objects:
#'
#' 1. [TaskClassifST]: Ensure the identity.
#' 2. [data.frame()] and [DataBackend]: Provides an alternative to the constructor of [TaskClassifST].
#' 3. [sf::sf]: Extracts spatial meta data before construction.
#' 4. [TaskRegr]: Calls [convert_task()].
#'
#' @inheritParams mlr3::as_task_classif
#' @template param_coords_as_features
#' @template param_crs
#' @template param_coordinate_names
#'
#' @return [TaskClassifST]
#' @export
as_task_classif_st = function(x, ...) {
  UseMethod("as_task_classif_st")
}

#' @rdname as_task_classif_st
#' @export as_task_classif_st.TaskClassifST
#' @exportS3Method
as_task_classif_st.TaskClassifST = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @rdname as_task_classif_st
#' @export as_task_classif_st.data.frame
#' @exportS3Method
as_task_classif_st.data.frame = function(x, target, id = deparse(substitute(x)), positive = NULL, coordinate_names, crs = NA_character_, coords_as_features = FALSE, label = NA_character_, ...) {
  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }
  TaskClassifST$new(id = id, backend = x, target = target, positive = positive, coords_as_features = coords_as_features, crs = crs, coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_classif_st
#' @export as_task_classif_st.DataBackend
#' @exportS3Method
as_task_classif_st.DataBackend = function(x, target, id = deparse(substitute(x)), positive = NULL, coordinate_names, crs, coords_as_features = FALSE, label = NA_character_, ...) {
  TaskClassifST$new(id = id, backend = x, target = target, positive = positive, coords_as_features = coords_as_features, crs = crs, coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_classif_st
#' @export as_task_classif_st.sf
#' @exportS3Method
as_task_classif_st.sf = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, coords_as_features = FALSE, label = NA_character_, ...) {
  id = as.character(id)
  geometries = as.character(unique(sf::st_geometry_type(x)))
  if (!test_names(geometries, identical.to = "POINT")) {
    stop("Simple feature may not contain geometries of type '%s'", str_collapse(setdiff(geometries, "POINT")))
  }

  # extract spatial meta data
  crs = sf::st_crs(x)$wkt
  coordinates = as.data.frame(sf::st_coordinates(x))
  coordinate_names = colnames(coordinates)

  # convert sf to data.frame
  x[[attr(x, "sf_column")]] = NULL
  attr(x, "sf_column") = NULL
  x = as.data.frame(x)

  # add coordinates
  x = cbind(x, coordinates)

  as_task_classif_st(x, target = target, id = id, positive = positive, coords_as_features = coords_as_features, crs = crs, coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_classif_st
#' @export as_task_classif_st.TaskRegrST
#' @exportS3Method
as_task_classif_st.TaskRegrST = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) {
  convert_task(intask = x, target = target, new_type = "classif_st", drop_original_target = FALSE, drop_levels = TRUE)
}
