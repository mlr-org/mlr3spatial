# Changelog

## mlr3spatial (development version)

## mlr3spatial 0.6.1

CRAN release: 2025-09-14

- compatibility: mlr3 1.2.0 removed the data format argument
- fix: `$predict_newdata()` and
  [`predict_spatial()`](https://mlr3spatial.mlr-org.com/dev/reference/predict_spatial.md)
  work without optional column roles like `coordinate` now.

## mlr3spatial 0.6.0

CRAN release: 2025-07-18

- fix: Throw an error when `X` and `Y` columns are present in an sf
  object

## mlr3spatial 0.5.0

CRAN release: 2024-03-09

- compatibility: Work with new paradox version 1.0.0

## mlr3spatial 0.4.1

CRAN release: 2023-05-03

- refactor: Use the
  [`terra::inMemory()`](https://rspatial.github.io/terra/reference/sources.html)
  function instead of `@ptr`.

## mlr3spatial 0.4.0

CRAN release: 2023-02-27

- refactor: The data input of `spatial_predict()` accepts `SpatRaster`,
  `stars` `sf` `RasterStack` and `RasterBrick` objects now. A
  [`mlr3::TaskUnsupervised`](https://mlr3.mlr-org.com/reference/TaskUnsupervised.html)
  can still be passed but the argument name changed from `task` to
  `newdata`.
- fix: The log showed a warning when the estimated values per chunk were
  a floating number.

## mlr3spatial 0.3.1

CRAN release: 2022-12-16

- chore: Remove `rgdal` dependency and require `raster` version 3.6-11.

## mlr3spatial 0.3.0

CRAN release: 2022-10-20

- feat: Add prediction on vector data to `spatial_predict()`.

## mlr3spatial 0.2.1

CRAN release: 2022-08-25

- fix: Add `"space"` and `"time"` column role from mlr3spatiotempcv

## mlr3spatial 0.2.0

CRAN release: 2022-08-18

- BREAKING CHANGE: `TaskClassifST` and `TaskRegrST` are used to train a
  learner with spatial data. The new tasks unify the work with
  mlr3spatiotempcv.
- BREAKING CHANGE: Raster objects cannot be used to create tasks for
  training anymore.
- BREAKING CHANGE: `TaskUnsupervised` is used to predict on rasters
  objects now. The new task type is more convenient for data without a
  response.
- feat: Add
  [`as_task_regr_st()`](https://mlr3spatial.mlr-org.com/dev/reference/as_task_regr_st.md)
  and
  [`as_task_classif_st()`](https://mlr3spatial.mlr-org.com/dev/reference/as_task_classif_st.md)
  from spatial objects.
- feat: Add
  [`as_task_unsupervised()`](https://mlr3.mlr-org.com/reference/as_task_unsupervised.html)
  from raster objects.
- feat: Task `leipzig` with land cover target.
- feat: `data("leipzig")` loads an `sf` object with land cover in
  Leipzig.
- feat: GeoTIFF and GeoPackage of Leipzig in `extdata` folder.
- refactor: Vector data is handled with `DataBackendDataTable` now and
  `DataBackendVector` is removed.
- BREAKING CHANGE: `DataBackendRaster` cannot be created from
  `RasterLayer` objects anymore.
- fix: `spatial_predict()` returned an unnamed response.
- fix: `spatial_predict()` wrote predictions to the wrong cell.
- BREAKING CHANGE: Remove `demo_raster()`, `demo_stack_spatraster()`,
  `demo_stack_rasterbrick()` and `demo_rasterbrick()` functions.
- feat: Prediction layer contains `NA` at raster cells with `NA` values
  in one or more feature layers.

## mlr3spatial 0.1.2

CRAN release: 2022-03-06

- refactor: Stars objects are directly converted to terra objects now.

## mlr3spatial 0.1.1

CRAN release: 2022-01-20

- fix: Compatibility to terra update.

## mlr3spatial 0.1.0

CRAN release: 2021-11-16

- First version of mlr3spatial.
