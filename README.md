# mlr3spatial

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3spatial/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3spatial/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/mlr3spatial)](https://CRAN.R-project.org/package=mlr3spatial)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

## Package scope

The handling of (large) spatial objects in ML is an error-prone and time consuming task.
Users often need to extract the "raw" values from the spatial objects (detaching the spatial metadata), train a model, predict and then recreate the spatial object again.
In addition, predictions on large raster files (i.e. multiple GB in size) often leads to memory issues on consumer grade machines.
{mlr3spatial} provides

- Spatial tasks ([`TaskClassifST`](https://mlr3spatial.mlr-org.com/dev/reference/TaskClassifST.html) and [`TaskRegrST`](https://mlr3spatial.mlr-org.com/dev/reference/TaskRegrST.html)) from [`sf`](https://CRAN.R-project.org/package=sf) vector objects to train a learner.
- Predict on spatial raster objects ([`terra`](https://CRAN.R-project.org/package=terra), [`raster`](https://CRAN.R-project.org/package=raster), [`stars`](https://CRAN.R-project.org/package=stars)), with optional parallelization and memory awareness.

## FAQ

<details>
  <summary>Will mlr3spatial support spatial learners?</summary>
  <br>
 Eventually. It is not yet clear whether these would live in mlr3extralearners or in {mlr3spatial}.
 So far there are none yet.
</details>

<details>
  <summary>Why are there two packages, mlr3spatial and mlr3spatiotempcv?</summary>
  <br>
  mlr3spatiotempcv is solely devoted to resampling techniques.
  There are quite a few and keeping packages small is one of the development philosophies of the mlr3 framework.
  Also back in the days when mlr3spatiotempcv was developed it was not yet clear how we want to structure additional spatial components such as prediction support for spatial classes and so on.
</details>
