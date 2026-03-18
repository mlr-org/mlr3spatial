# Convert to a Spatiotemporal Classification Task

Convert object to a
[TaskClassifST](https://mlr3spatial.mlr-org.com/dev/reference/TaskClassifST.md).
This is a S3 generic, specialized for at least the following objects:

1.  [TaskClassifST](https://mlr3spatial.mlr-org.com/dev/reference/TaskClassifST.md):
    Ensure the identity.

2.  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) and
    [mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html):
    Provides an alternative to the constructor of
    [TaskClassifST](https://mlr3spatial.mlr-org.com/dev/reference/TaskClassifST.md).

3.  [sf::sf](https://r-spatial.github.io/sf/reference/sf.html): Extracts
    spatial meta data before construction.

4.  [mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html):
    Calls
    [`mlr3::convert_task()`](https://mlr3.mlr-org.com/reference/convert_task.html).

## Usage

``` r
as_task_classif_st(x, ...)

# S3 method for class 'TaskClassifST'
as_task_classif_st(x, clone = FALSE, ...)

# S3 method for class 'data.frame'
as_task_classif_st(
  x,
  target,
  id = deparse(substitute(x)),
  positive = NULL,
  coordinate_names,
  crs = NA_character_,
  coords_as_features = FALSE,
  label = NA_character_,
  ...
)

# S3 method for class 'DataBackend'
as_task_classif_st(
  x,
  target,
  id = deparse(substitute(x)),
  positive = NULL,
  coordinate_names,
  crs,
  coords_as_features = FALSE,
  label = NA_character_,
  ...
)

# S3 method for class 'sf'
as_task_classif_st(
  x,
  target = NULL,
  id = deparse(substitute(x)),
  positive = NULL,
  coords_as_features = FALSE,
  label = NA_character_,
  ...
)

# S3 method for class 'TaskRegrST'
as_task_classif_st(
  x,
  target = NULL,
  drop_original_target = FALSE,
  drop_levels = TRUE,
  ...
)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Additional arguments.

- clone:

  (`logical(1)`)  
  If `TRUE`, ensures that the returned object is not the same as the
  input `x`.

- target:

  (`character(1)`)  
  Name of the target column.

- id:

  (`character(1)`)  
  Id for the new task. Defaults to the (deparsed and substituted) name
  of the data argument.

- positive:

  (`character(1)`)  
  Level of the positive class. See
  [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).

- coordinate_names:

  (`character(1)`)  
  The column names of the coordinates in the data.

- crs:

  (`character(1)`)  
  Coordinate reference system. WKT2 or EPSG string.

- coords_as_features:

  (`logical(1)`)  
  If `TRUE`, coordinates are used as features. This is a shortcut for
  `task$set_col_roles(c("x", "y"), role = "feature")` with the
  assumption that the coordinates in the data are named "x" and "y".

- label:

  (`character(1)`)  
  Label for the new instance.

- drop_original_target:

  (`logical(1)`)  
  If `FALSE` (default), the original target is added as a feature.
  Otherwise the original target is dropped.

- drop_levels:

  (`logical(1)`)  
  If `TRUE` (default), unused levels of the new target variable are
  dropped.

## Value

[TaskClassifST](https://mlr3spatial.mlr-org.com/dev/reference/TaskClassifST.md)
