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
library(raster)
library(mlr3)
library(mlr3raster)
library(data.table)

stack = demo_stack()
writeRaster(stack, "demo_stack_500mb.tif")
rm(stack)
```

`demo_stack` generates a raster stack with 5 layers (4 predictor
variables and 1 response variable) with a total size of 1GB. The file is
written to disk and the stack is removed to free up memory.

``` r
data = as.data.table(sampleRandom(stack("demo_stack_500mb.tif"), 500))
names(data) = paste0("var", 1:5)
data = data[, var1:=as.factor(var1)]
```

In order to fit a model, 500 raster cells are randomly sampled. The
response variable returned by `demo_stack` is located in the first
column of `data`.

``` r
task = TaskClassif$new(id = "raster", backend = data, target = "var1", positive = "1")

learner_svm = LearnerClassifSVMParallel$new()
learner_svm$train(task, row_ids = 1:500)
```

A mlr3 `task` is created and a classification svm is fitted with the
randomly sampled raster cells.

``` r
data_stack = stack("inst/demo_stack_500mb.tif")
names(data_stack) = paste0("var", 1:5)
data_stack = dropLayer(data_stack, 1)

reclassify_table = data.table(task = c(0,1), raster = c(10, 11))
pred = PredictionRasterClassif$new(data_stack, task, reclassify_table)
pred$chunksize = 100
```

The `PredictionRasterClassif` class controls the splitting of the raster
stack into chunks. The most important parameter is the `chunksize` which
controls size of processed raster chunks. The automatic detection of
this parameter is error-prone since different `future` plans and `mlr3`
learners will consume a highly varying amount of memory.

``` r
future::plan("multisession")

ras = pred$predict(learner_svm)
```

Execute the raster prediction in parallel.

