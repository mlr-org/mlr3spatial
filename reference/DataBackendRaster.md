# DataBackend for Raster Objects

[mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html)
for
[terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
raster objects.

## Read mode

There are two different ways the reading of values is performed
internally:

- "Block mode" reads complete rows of the raster file and subsets the
  requested cells. This mode is faster than "cell mode" if the complete
  raster file is iterated over.

- "Cell mode" reads individual cells. This is faster than "block mode"
  if only a few cells are sampled.

"Block mode" is activated if `$data(rows)` is used with a increasing
integer sequence e.g. `200:300`. If only a single cell is requested,
"cell mode" is used.

## Super class

[`mlr3::DataBackend`](https://mlr3.mlr-org.com/reference/DataBackend.html)
-\> `DataBackendRaster`

## Active bindings

- `rownames`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Returns vector of all distinct row identifiers, i.e. the contents of
  the primary key column.

- `colnames`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns vector of all column names.

- `nrow`:

  (`integer(1)`)  
  Number of rows (observations).

- `ncol`:

  (`integer(1)`)  
  Number of columns (variables).

- `stack`:

  (`SpatRaster`)  
  Raster stack.

## Methods

### Public methods

- [`DataBackendRaster$new()`](#method-DataBackendRaster-initialize)

- [`DataBackendRaster$data()`](#method-DataBackendRaster-data)

- [`DataBackendRaster$head()`](#method-DataBackendRaster-head)

- [`DataBackendRaster$distinct()`](#method-DataBackendRaster-distinct)

- [`DataBackendRaster$missings()`](#method-DataBackendRaster-missings)

- [`DataBackendRaster$coordinates()`](#method-DataBackendRaster-coordinates)

Inherited methods

- [`mlr3::DataBackend$format()`](https://mlr3.mlr-org.com/reference/DataBackend.html#method-format)
- [`mlr3::DataBackend$print()`](https://mlr3.mlr-org.com/reference/DataBackend.html#method-print)

------------------------------------------------------------------------

### `DataBackendRaster$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    DataBackendRaster$new(data)

#### Arguments

- `data`:

  ([terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html))  
  The input
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

------------------------------------------------------------------------

### `DataBackendRaster$data()`

Returns a slice of the raster in the specified format. Currently, the
only supported formats is `"data.table"`.

The rows must be addressed as vector of cells indices, columns must be
referred to via layer names. Queries for rows with no matching row id
and queries for columns with no matching column name are silently
ignored.

Rows are guaranteed to be returned in the same order as `rows`, columns
may be returned in an arbitrary order. Duplicated row ids result in
duplicated rows, duplicated column names lead to an exception.

#### Usage

    DataBackendRaster$data(rows, cols)

#### Arguments

- `rows`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Row indices. Row indices start with 1 in the upper left corner in the
  raster, increase from left to right and then from top to bottom. The
  last cell is in the bottom right corner and the row index equals the
  number of cells in the raster.

- `cols`:

  [`character()`](https://rdrr.io/r/base/character.html)  
  Column names.

------------------------------------------------------------------------

### `DataBackendRaster$head()`

Retrieve the first `n` rows.

#### Usage

    DataBackendRaster$head(n = 6L)

#### Arguments

- `n`:

  (`integer(1)`)  
  Number of rows.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
of the first `n` rows.

------------------------------------------------------------------------

### `DataBackendRaster$distinct()`

Returns a named list of vectors of distinct values for each column
specified. If `na_rm` is `TRUE`, missing values are removed from the
returned vectors of distinct values. Non-existing rows and columns are
silently ignored.

#### Usage

    DataBackendRaster$distinct(rows, cols, na_rm = TRUE)

#### Arguments

- `rows`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Row indices. Row indices start with 1 in the upper left corner in the
  raster, increase from left to right and then from top to bottom. The
  last cell is in the bottom right corner and the row index equals the
  number of cells in the raster.

- `cols`:

  [`character()`](https://rdrr.io/r/base/character.html)  
  Column names.

- `na_rm`:

  `logical(1)`  
  Whether to remove NAs or not.

#### Returns

Named [`list()`](https://rdrr.io/r/base/list.html) of distinct values.

------------------------------------------------------------------------

### `DataBackendRaster$missings()`

Returns the number of missing values per column in the specified slice
of data. Non-existing rows and columns are silently ignored.

#### Usage

    DataBackendRaster$missings(rows, cols)

#### Arguments

- `rows`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Row indices. Row indices start with 1 in the upper left corner in the
  raster, increase from left to right and then from top to bottom. The
  last cell is in the bottom right corner and the row index equals the
  number of cells in the raster.

- `cols`:

  [`character()`](https://rdrr.io/r/base/character.html)  
  Column names.

#### Returns

Total of missing values per column (named
[`numeric()`](https://rdrr.io/r/base/numeric.html)).

------------------------------------------------------------------------

### `DataBackendRaster$coordinates()`

Returns the coordinates of `rows`. If `rows` is missing, all coordinates
are returned.

#### Usage

    DataBackendRaster$coordinates(rows)

#### Arguments

- `rows`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Row indices. Row indices start with 1 in the upper left corner in the
  raster, increase from left to right and then from top to bottom. The
  last cell is in the bottom right corner and the row index equals the
  number of cells in the raster.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
of coordinates of `rows`.
