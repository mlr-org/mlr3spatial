
# mlr3spatial <img src="man/figures/logo.png" align="right" width = "120" />

Package website: [release](https://mlr3spatial.mlr-org.com/) |
[dev](https://mlr3spatial.mlr-org.com/dev/)

<!-- badges: start -->

[![r-cmd-check](https://github.com/mlr-org/mlr3spatial/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3spatial/actions/workflows/r-cmd-check.yml)
[![CRAN
status](https://www.r-pkg.org/badges/version/mlr3spatial)](https://CRAN.R-project.org/package=mlr3spatial)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

*mlr3spatial* is the package for spatial objects within the
[`mlr3`](https://mlr-org.com) ecosystem. The package directly loads data
from [`sf`](https://CRAN.R-project.org/package=sf) objects to train any
mlr3 learner. The learner can predict on various raster formats
([`terra`](https://CRAN.R-project.org/package=terra),
[`raster`](https://CRAN.R-project.org/package=raster) and
[`stars`](https://CRAN.R-project.org/package=stars)) and writes the
prediction raster to disk. mlr3spatial reads large raster objects in
chunks to avoid memory issues and predicts the chunks in parallel. Check
out [`mlr3spatiotempcv`](https://github.com/mlr-org/mlr3spatiotempcv)
for spatiotemporal resampling within mlr3.

## Resources

There are sections about spatial data in the
[mlr3book](https://mlr3book.mlr-org.com).

  - Learn how to
    [predict](https://mlr3book.mlr-org.com/special.html#sec-spatial-prediction)
    a spatial raster image.
  - Estimate the performance of a model with [spatial
    cross-validation](https://mlr3book.mlr-org.com/special.html#spatiotemp-cv).

The gallery features articles about spatial data in the mlr3 ecosystem.

  - Learn the basics with a [land cover
    classification](https://mlr-org.com/gallery/technical/2023-02-27-land-cover-classification/)
    of the city of Leipzig.

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3spatial")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3spatial")
```

## Example

Our goal is to map the land cover of the city of Leipzig. The
`mlr3spatial` package contains a Sentinel-2 scene of the city of Leipzig
and a point vector with training sites. The Sentinel-2 scene is a 10m
resolution multispectral image with 7 bands and the NDVI. The points
represent samples of the four land cover classes: Forest, Pastures,
Urban and Water. We load the raster with the
[`terra`](https://CRAN.R-project.org/package=terra) package and the
vector with the [`sf`](https://CRAN.R-project.org/package=sf) package in
the R Session.

``` r
library(mlr3verse)
library(mlr3spatial)
library(terra, exclude = "resample")
library(sf)

leipzig = read_sf(system.file("extdata", "leipzig_points.gpkg", package = "mlr3spatial"), stringsAsFactors = TRUE)

leipzig_raster = rast(system.file("extdata", "leipzig_raster.tif", package = "mlr3spatial"))
```

The function `as_task_classif_st()` converts the `sf::sf` object to a
spatial classification task.

``` r
task = as_task_classif_st(leipzig, target = "land_cover")
task
```

    ## <TaskClassifST:leipzig> (97 x 9)
    ## * Target: land_cover
    ## * Properties: multiclass
    ## * Features (8):
    ##   - dbl (8): b02, b03, b04, b06, b07, b08, b11, ndvi
    ## * Coordinates:
    ##            X       Y
    ##  1: 732480.1 5693957
    ##  2: 732217.4 5692769
    ##  3: 732737.2 5692469
    ##  4: 733169.3 5692777
    ##  5: 732202.2 5692644
    ## ---                 
    ## 93: 733018.7 5692342
    ## 94: 732551.4 5692887
    ## 95: 732520.4 5692589
    ## 96: 732542.2 5692204
    ## 97: 732437.8 5692300

The points are located in the district of Lindenau and Zentrum-West.

<img src="man/figures/sentinel.png" />

Now we train a classification tree on the leipzig task.

``` r
learner = lrn("classif.rpart")
learner$train(task)
```

As a last step, we predict the land cover class for the whole area of
interest. For this, we pass the Sentinel-2 scene and the trained learner
to the `predict_spatial()` function.

``` r
land_cover = predict_spatial(leipzig_raster, learner)
```

<img src="man/figures/land_cover.png" />

## FAQ

<details>

<summary>Will mlr3spatial support spatial learners?</summary> <br>
Eventually. It is not yet clear whether these would live in
mlr3extralearners or in mlr3spatial. So far there are none yet.

</details>

<details>

<summary>Why are there two packages, mlr3spatial and
mlr3spatiotempcv?</summary> <br> mlr3spatiotempcv is solely devoted to
resampling techniques. There are quite a few and keeping packages small
is one of the development philosophies of the mlr3 framework. Also back
in the days when mlr3spatiotempcv was developed, it was not yet clear
how we want to structure additional spatial components such as
prediction support for spatial classes and so on.

</details>
