# mlr3spatial

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3spatial/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3spatial/actions)
[![Coverage status](https://codecov.io/gh/mlr-org/mlr3spatial/branch/main/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3spatial?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3spatial/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3spatial)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package is **NOT** yet ready for production use.
Functions may change without notice!

## Package scope

The handling of (large) spatial objects ({terra}, {raster}, {stars}, {sf}) in ML is an error-prone and time consuming task.
Users often need to extract the "raw" values from the spatial objects, train a model, predict and then recreate the spatial object again.
{mlr3spatial} tries to help here by

- Providing `DataBackend` classes for various spatial classes ({terra}, {raster}, {stars}, {sf}) which can be used to easily create {mlr3} [Tasks](https://mlr3.mlr-org.com/reference/Task.html) 
- Support for direct predictions to spatial objects ({terra}, {raster}, {stars}, {sf}), returning both a {mlr3} [Prediction](https://mlr3.mlr-org.com/reference/Prediction.html) object and the respective spatial object
- Speeding up predictions by making use of {mlr3} built-in future-based parallelization

## Spatiotemporal resampling / cross-validation

For spatiotemporal resampling within mlr3 see [{mlr3spatiotempcv}](https://github.com/mlr-org/mlr3spatiotempcv).
