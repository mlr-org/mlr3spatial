# DataBackend for Vector Objects

[mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html)
for [sf::sf](https://r-spatial.github.io/sf/reference/sf.html) vector
objects.

## Super classes

[`mlr3::DataBackend`](https://mlr3.mlr-org.com/reference/DataBackend.html)
-\>
[`mlr3::DataBackendDataTable`](https://mlr3.mlr-org.com/reference/DataBackendDataTable.html)
-\> `DataBackendVector`

## Active bindings

- `sfc`:

  ([sf::sfc](https://r-spatial.github.io/sf/reference/sfc.html))  
  Returns the sfc object.

## Methods

### Public methods

- [`DataBackendVector$new()`](#method-DataBackendVector-new)

Inherited methods

- [`mlr3::DataBackend$format()`](https://mlr3.mlr-org.com/reference/DataBackend.html#method-format)
- [`mlr3::DataBackend$print()`](https://mlr3.mlr-org.com/reference/DataBackend.html#method-print)
- [`mlr3::DataBackendDataTable$data()`](https://mlr3.mlr-org.com/reference/DataBackendDataTable.html#method-data)
- [`mlr3::DataBackendDataTable$distinct()`](https://mlr3.mlr-org.com/reference/DataBackendDataTable.html#method-distinct)
- [`mlr3::DataBackendDataTable$head()`](https://mlr3.mlr-org.com/reference/DataBackendDataTable.html#method-head)
- [`mlr3::DataBackendDataTable$missings()`](https://mlr3.mlr-org.com/reference/DataBackendDataTable.html#method-missings)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    DataBackendVector$new(data, primary_key)

#### Arguments

- `data`:

  (`sf`)  
  A raster object.

- `primary_key`:

  (`character(1)` \|
  [`integer()`](https://rdrr.io/r/base/integer.html))  
  Name of the primary key column, or integer vector of row ids.
