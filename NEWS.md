# mlr3spatial 0.1.2.9000

* feat: `data("leipzig")` loads an `sf` object with land cover in Leipzig.
* feat: GeoTIFF and GeoPackage of Leipzig in `extdata` folder.
* feat: Task `leipzig` with land cover target.
* refactor: `DataBackendVector` inherits from `DataBackendDataTable` now.
* BREAKING CHANGE: `DataBackendRaster` cannot be created from `RasterLayer` objects anymore.
* fix: `DataBackendVector` initialization failed when geometry column had a different name than `geometry`.
* fix: `spatial_predict()` returned an unnamed response.
* feat: Add `as_task_regr()` and `as_task_classif()` from spatial objects.
* fix: `spatial_predict()` wrote predictions to the wrong cell.
* BREAKING CHANGE: Remove `demo_raster()`, `demo_stack_spatraster()`, `demo_stack_rasterbrick()` and `demo_rasterbrick()` functions.
* feat: Prediction layer contains `NA` at raster cells with `NA` values in one or more feature layers.

# mlr3spatial 0.1.2

* refactor: stars objects are directly converted to terra objects now.

# mlr3spatial 0.1.1

* fix: compatibility to `terra` update.

# mlr3spatial 0.1.0

* First version of mlr3spatial.

