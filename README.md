# mlr3spatial

<!-- badges: start -->
[![tic](https://github.com/mlr-org/mlr3spatial/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3spatial/actions)
[![Coverage status](https://codecov.io/gh/mlr-org/mlr3spatial/branch/main/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3spatial?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3spatial/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3spatial)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package is **NOT** ready for production use and only a proof of concept for now.
For spatiotemporal resampling within mlr3 see [{mlr3spatiotempcv}](https://github.com/mlr-org/mlr3spatiotempcv).

# Package scope

The handling of large spatial objects (terra, raster, stars) in ML is an error-prone and time consuming task.

This package aims to

1. Simplify Task and Prediction handling of spatial objects within the mlr3 ecosystem
1. Prediction of spatial objects in chunks to prevent memory overflow
1. (optional) Speeding up the prediction process by predicting in parallel, making use of the built-in parallelization in the mlr3 ecosystem (via future)
