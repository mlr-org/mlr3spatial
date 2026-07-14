# Sample Points in Raster Stack

Samples `n` points of a raster stack.

## Usage

``` r
sample_stack(stack, n = 100)
```

## Arguments

- stack:

  ([terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html))  
  Raster stack.

- n:

  (`integer(1)`)  
  Number of points.

## Value

[sf::sf](https://r-spatial.github.io/sf/reference/sf.html)
