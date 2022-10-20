# mlr3spatial 0.3.0

* feat: add prediction on vector data to `spatial_predict()`.

# mlr3spatial 0.2.1

* fix: add `"space"` and `"time"` column role from mlr3spatiotempcv

# mlr3spatial 0.2.0

* BREAKING CHANGE: `TaskClassifST` and `TaskRegrST` are used to train a learner with spatial data.
  The new tasks unify the work with mlr3spatiotempcv.
* BREAKING CHANGE: Raster objects cannot be used to create tasks for training anymore.
* BREAKING CHANGE: `TaskUnsupervised` is used to predict on rasters objects now.
  The new task type is more convenient for data without a response.
* feat: Add `as_task_regr_st()` and `as_task_classif_st()` from spatial objects.
* feat: Add `as_task_unsupervised()` from raster objects.
* feat: Task `leipzig` with land cover target.
* feat: `data("leipzig")` loads an `sf` object with land cover in Leipzig.
* feat: GeoTIFF and GeoPackage of Leipzig in `extdata` folder.
* refactor: Vector data is handled with `DataBackendDataTable` now and `DataBackendVector` is removed.
* BREAKING CHANGE: `DataBackendRaster` cannot be created from `RasterLayer` objects anymore.
* fix: `spatial_predict()` returned an unnamed response.
* fix: `spatial_predict()` wrote predictions to the wrong cell.
* BREAKING CHANGE: Remove `demo_raster()`, `demo_stack_spatraster()`, `demo_stack_rasterbrick()` and `demo_rasterbrick()` functions.
* feat: Prediction layer contains `NA` at raster cells with `NA` values in one or more feature layers.

# mlr3spatial 0.1.2

* refactor: stars objects are directly converted to terra objects now.

# mlr3spatial 0.1.1

* fix: compatibility to terra update.

# mlr3spatial 0.1.0

* First version of mlr3spatial.

