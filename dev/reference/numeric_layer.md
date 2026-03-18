# Numeric Layer Generator

Generates a numeric layer when passed to
[`generate_stack()`](https://mlr3spatial.mlr-org.com/dev/reference/generate_stack.md).

## Usage

``` r
numeric_layer(id, in_memory = FALSE)
```

## Arguments

- id:

  (`character(1)`)  
  Layer id.

- in_memory:

  (`logical(1)`)  
  If `FALSE` (default), layer is written to disk.

## Value

Named [`list()`](https://rdrr.io/r/base/list.html)
