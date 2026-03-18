# Factor Layer Generator

Generates a factor layer when passed to
[`generate_stack()`](https://mlr3spatial.mlr-org.com/dev/reference/generate_stack.md).

## Usage

``` r
factor_layer(id, levels, in_memory = FALSE)
```

## Arguments

- id:

  (`character(1)`)  
  Layer id.

- levels:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Factor levels.

- in_memory:

  (`logical(1)`)  
  If `FALSE` (default), layer is written to disk.

## Value

Named [`list()`](https://rdrr.io/r/base/list.html)
