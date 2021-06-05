
# mlr3spatial

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3spatial/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3spatial/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/mlr3spatial)](https://cran.r-project.org/package=mlr3spatial)
[![Coverage
status](https://codecov.io/gh/mlr-org/mlr3spatial/branch/main/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3spatial?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3spatial/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3spatial)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package is **not** ready for production use and only a proof of
concept for now. For spatiotemporal resampling support see
[mlr3spatiotempcv](https://github.com/mlr-org/mlr3spatiotempcv).

# Proof of Concept

## Introduction

The prediction of large raster files is an error-prone and time
consuming task in R.

This package aims to

1.  Allow raster prediction with mlr3 learners
2.  Predict raster files in chunks to prevent memory related errors
3.  Speed up the prediction process by predicting the chunks in parallel

## Methods

The size of raster files in memory can be easily calculated with `number
of cells * number of layers * 8` since one raster cell allocates 8
bytes. However, the memory usage during the prediction process highly
varies between different `futures` and the used learning algorithm. For
example, the `multisession` future generates multiple R sessions and
creates copy of the variables for each session. It is even harder to
predict how much memory the learning algorithm will need for the
prediction. Therefore, we decided that the chunk size should be manually
set by user to avoid an error-prone automatic detection of the optimal
chunk size.

## Example

The package offers functions to create demo raster files in any size.

``` r
library(mlr3spatial)

stack = demo_stack(size = 500, layers=5)
writeRaster(stack, "demo_stack_500mb.tif", overwrite = TRUE)
rm(stack)
```

`demo_stack` generates a raster stack with 5 layers (4 predictor
variables and 1 response variable) with a total size of 500MB. The file
is written to disk and the stack is removed to free up memory.

``` r
stack = rast("demo_stack_500mb.tif")
data_train = as.data.table(spatSample(stack, 500))
data_train[, y:=as.factor(y)]
```

In order to fit a model, 500 raster cells are randomly sampled. The
response variable returned by `demo_stack` is located in the first
column of `data`.

``` r
task = as_task_classif(data_train, target = "y", positive = "1")

learner_svm = LearnerClassifSVMParallel$new()
learner_svm$train(task)
```

A mlr3 `task` is created and a classification svm is fitted with the
randomly sampled raster cells.

``` r
data_predict = subset(stack, 1:4)
prediction_raster = PredictionRaster$new(data_predict, chunksize = 100)
```

The `PredictionRaster` class controls the splitting of the raster stack
into chunks. The most important parameter is the `chunksize` which
controls size of processed raster chunks. The automatic detection of
this parameter is error-prone since different `future` plans and `mlr3`
learners will consume a highly varying amount of memory.

``` r
future::plan("multisession")

ras = prediction_raster$predict(learner_svm)
```

Execute the raster prediction in parallel.
