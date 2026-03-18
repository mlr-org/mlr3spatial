# Generate Raster Stack

Generates a raster stack.

## Usage

``` r
generate_stack(
  layers,
  layer_size = NULL,
  dimension = NULL,
  multi_layer_file = FALSE
)
```

## Arguments

- layers:

  (List of
  [`numeric_layer()`](https://mlr3spatial.mlr-org.com/dev/reference/numeric_layer.md)
  and
  [`factor_layer()`](https://mlr3spatial.mlr-org.com/dev/reference/factor_layer.md))  
  List of layers.

- layer_size:

  (`numeric(1)`)  
  Size of a single layer in megabytes.

- dimension:

  (`integer(1)`)  
  Dimension of the squared layers.

- multi_layer_file:

  (`logical(1)`)  
  If `TRUE`, raster is written to disk as a single multi-layer file.
  Overwrites `ìn_memory` argument of
  [`numeric_layer()`](https://mlr3spatial.mlr-org.com/dev/reference/numeric_layer.md)
  and
  [`factor_layer()`](https://mlr3spatial.mlr-org.com/dev/reference/factor_layer.md).

  `layer_size` and `dimension` are mutually exclusive.

## Value

[terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
