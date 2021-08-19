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

## FAQ

<details>
  <summary>Do I need to use <code>TaskClassifST</code>/<code>TaskRegrST</code> with {mlr3spatial}?</summary>
  No, you can use `TaskClassif` and `TaskRegr`. However, their `*ST` equivalents will also work.
  When we introduced the `*ST` tasks, we had no support for spatial backends yet and there was a need to store the spatial information somewhere.
</details>

<details>
  <summary>Why is {mlr3spatial} faster when predicting than doing it directly via the spatial packages?</summary>
  {mlr3spatial} makes use of the parallel prediction heuristic within {mlr3}.
  This one makes use of the {future} and {data.table} packages for parallelization and data handling.
  If {mlr3spatial} is faster, than this way seems to be more efficient than the parallelization built into the respective other packages.
  In theory the overhead in {mlr3spatial} should be higher because we extract the values from the spatial objects first.
</details>

<details>
  <summary>Can I make use of parallel predictions during nested resampling/tuning?</summary>
  In theory yes, {mlr3} supports nested parallelization via the {future} framework.
  Watch out for required resources when having multiple parallelized layers.
</details>

<details>
  <summary>Will {mlr3spatial} support spatial learners?</summary>
 Eventually. It is not yet clear whether these would live in {mlr3extralearners} or in {mlr3spatial}.
 So far there are none yet.
</details>

<details>
  <summary>Why can I only predict to "newdata" and not use a subset of the task?</summary>
  Most often spatial data is stored in TIFF, Geopackage or Shapefiles.
  Passing these as "newdata" directly into the `predict()` call is what is most often done in practice.
  When creating a spatial backend it is often hard to distinguish train and predict parts upfront.
  In addition, this requires a subset call of `task$data()` internally - which comes with some trouble for specific backends such as `terra::SpatRaster` due to "external pointer" issues when going parallel.
  For these reasons (and to avoid headaches in the first place) we decided to only support "newdata" prediction for the moment.
</details>

<details>
  <summary>Why are there two packages, {mlr3spatial} and {mlr3spatiotempcv}?</summary>
  {mlr3spatiotempcv} is solely devoted to resampling techniques.
  There are quite a few and keeping packages small is one of the development philosophies of the mlr3 framework.
  Also back in the days when {mlr3spatiotempcv} was developed it was not yet clear how we want to structure additional spatial components such as prediction support for spatial classes and so on.
</details>
