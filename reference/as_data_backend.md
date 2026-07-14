# Coerce to spatial DataBackend

Wraps a
[mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html)
around spatial objects. Currently these S3 methods are only alternative
ways for writing `DataBackendRaster$new()`. They do not support coercing
from other backends yet.

## Usage

``` r
# S3 method for class 'stars'
as_data_backend(data, primary_key = NULL, ...)

# S3 method for class 'SpatRaster'
as_data_backend(data, primary_key = NULL, ...)

# S3 method for class 'RasterBrick'
as_data_backend(data, primary_key = NULL, ...)

# S3 method for class 'RasterStack'
as_data_backend(data, primary_key = NULL, ...)

# S3 method for class 'sf'
as_data_backend(data, primary_key = NULL, keep_rownames = FALSE, ...)
```

## Arguments

- data:

  ([terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html))  
  The input
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

- primary_key:

  (`character(1)` \|
  [`integer()`](https://rdrr.io/r/base/integer.html))  
  Name of the primary key column, or integer vector of row ids.

- ...:

  (`any`)  
  Not used.

- keep_rownames:

  (`logical(1)` \| `character(1)`)  
  If `TRUE` or a single string, keeps the row names of `data` as a new
  column. The column is named like the provided string, defaulting to
  `"..rownames"` for `keep_rownames == TRUE`. Note that the created
  column will be used as a regular feature by the task unless you
  manually change the column role. Also see
  [`data.table::as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).

## Value

[mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html).
