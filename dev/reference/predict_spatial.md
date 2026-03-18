# Predict on Spatial Objects with mlr3 Learners

This function allows to directly predict mlr3 learners on various
spatial objects.

## Usage

``` r
predict_spatial(
  newdata,
  learner,
  chunksize = 200L,
  format = "terra",
  filename = NULL
)
```

## Arguments

- newdata:

  ([terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  \| `stars::stars` \|
  [sf::sf](https://r-spatial.github.io/sf/reference/sf.html) \|
  `raster::RasterStack` \| `raster::RasterBrick`). New data to predict
  on. All spatial data formats convertible by
  [`as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.html)
  are supported e.g.
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [sf::sf](https://r-spatial.github.io/sf/reference/sf.html).

- learner:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)).
  Learner with trained model.

- chunksize:

  (`integer(1)`)  
  The chunksize determines in how many subparts the prediction task will
  be split into. The value can be roughly thought of as megabyte of a
  raster file on disk. For example, if a prediction on a 1 GB file would
  be carried out with `chunksize = 100L`, the prediction would happen in
  10 chunks.

  The default of `chunksize = 1000L` might be a good compromise between
  speed and memory usage. If you find yourself running out of memory,
  reduce this value.

- format:

  (`character(1)`)  
  Output class of the resulting object. Accepted values are `"raster"`,
  `"stars"` and `"terra"` if the input is a raster. Note that when
  choosing something else than `"terra"`, the spatial object is
  converted into the respective format which might cause overhead both
  in runtime and memory allocation. For vector data only `"sf"` is
  supported.

- filename:

  (`character(1)`)  
  Path where the spatial object should be written to.

## Value

Spatial object of class given in argument `format`.

## Examples

``` r
library(terra, exclude = "resample")
#> terra 1.9.1

# fit rpart on training points
task_train = tsk("leipzig")
learner = lrn("classif.rpart")
learner$train(task_train)

# load raster
stack = rast(system.file("extdata", "leipzig_raster.tif", package = "mlr3spatial"))

# predict land cover classes
pred = predict_spatial(stack, learner, chunksize = 1L)
```
