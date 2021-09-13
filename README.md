# mlr3spatial

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3spatial/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3spatial/actions)
[![Coverage status](https://codecov.io/gh/mlr-org/mlr3spatial/branch/main/graph/badge.svg)](https://codecov.io/github/mlr-org/mlr3spatial?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3spatial/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3spatial)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Package scope

The handling of (large) spatial objects ([{terra}](https://cran.r-project.org/web/packages/terra/index.html), [{raster}](https://cran.r-project.org/web/packages/raster/index.html), [{stars}](https://cran.r-project.org/web/packages/stars/index.html), [{sf}](https://cran.r-project.org/web/packages/sf/index.html) in ML is an error-prone and time consuming task.
Users often need to extract the "raw" values from the spatial objects (detaching the spatial metadata), train a model, predict and then recreate the spatial object again.  
In addition, predictions on large raster files (i.e. multiple GB in size) often leads to memory issues on consumer grade machines.
{mlr3spatial} tries to help here by

- Providing a [`DataBackendRaster`](https://mlr3.mlr-org.com/reference/DataBackend.html) class for {mlr3} which is able to handle various spatial raster classes ([{terra}](https://cran.r-project.org/web/packages/terra/index.html), [{raster}](https://cran.r-project.org/web/packages/raster/index.html), [{stars}](https://cran.r-project.org/web/packages/stars/index.html))
- Providing a [`DataBackendVector`](https://mlr3.mlr-org.com/reference/DataBackend.html) class for {mlr3} which is able to handle [{sf}](https://cran.r-project.org/web/packages/sf/index.html) objects
- Support for enhanced predictions on spatial objects ([{terra}](https://cran.r-project.org/web/packages/terra/index.html), [{raster}](https://cran.r-project.org/web/packages/raster/index.html), [{stars}](https://cran.r-project.org/web/packages/stars/index.html), [{sf}](https://cran.r-project.org/web/packages/sf/index.html), with optional **parallelization** and **memory awareness**

## Spatiotemporal resampling / cross-validation

For spatiotemporal resampling within mlr3 see [{mlr3spatiotempcv}](https://github.com/mlr-org/mlr3spatiotempcv).

## FAQ

<details>
  <summary>Do I need to use <code>TaskClassifST</code>/<code>TaskRegrST</code> with {mlr3spatial}?</summary>
  <br>
    No, you can use `TaskClassif` and `TaskRegr`. However, their `*ST` equivalents will also work.
    When we introduced the `*ST` tasks, we had no support for spatial backends yet and there was a need to store the spatial information somewhere.
</details>

<details>
  <summary>Why is {mlr3spatial} faster when predicting compared to the native spatial packages?</summary>
  <br>
  {mlr3spatial} makes use of the parallel prediction heuristic within {mlr3}.
  This one makes use of the {future} and {data.table} packages for parallelization and data handling.
  If {mlr3spatial} is faster, than this way seems to be more efficient than the parallelization built into the respective other packages.
</details>

<details>
  <summary>Can I make use of parallel predictions during nested resampling/tuning?</summary>
  <br>
  Yes, {mlr3} supports (nested) parallelization via the {future} framework.
  Watch out for required resources when having multiple parallelized layers.
</details>

<details>
  <summary>Will {mlr3spatial} support spatial learners?</summary>
  <br>
 Eventually. It is not yet clear whether these would live in {mlr3extralearners} or in {mlr3spatial}.
 So far there are none yet.
</details>

<details>
  <summary>Why are there two packages, {mlr3spatial} and {mlr3spatiotempcv}?</summary>
  <br>
  {mlr3spatiotempcv} is solely devoted to resampling techniques.
  There are quite a few and keeping packages small is one of the development philosophies of the mlr3 framework.
  Also back in the days when {mlr3spatiotempcv} was developed it was not yet clear how we want to structure additional spatial components such as prediction support for spatial classes and so on.
</details>

## Articles

- [Getting Started](https://mlr3spatial.mlr-org.com/articles/mlr3spatial.html)
- [Benchmarking parallel predictions](https://mlr3spatial.mlr-org.com/articles/benchmark.html)
