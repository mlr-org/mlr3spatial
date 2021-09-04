# stack_classif_terra = demo_stack_spatraster(size = 1, layers = 5)
# backend_terra = DataBackendSpatRaster$new(stack_classif_terra)
#
# stack_classif_raster = demo_stack_rasterbrick(size = 1, layers = 5)
# backend_raster = DataBackendRasterBrick$new(stack_classif_raster, response = "y",
#   response_is_factor = TRUE)
#
# backend_sf = as_sf_backend(backend_raster)
# sf_pred = sf::st_as_sf(raster::rasterToPoints(stack_classif_raster, spatial = TRUE))
#
# tif = system.file("tif/L7_ETMs.tif", package = "stars")
# l7data = stars::read_stars(tif)
# # l7data[ ,c('x', 'y')] <- list(NULL)
# backend_stars = as_stars_backend(l7data, quiet = TRUE)
# backend_stars_classif = as_stars_backend(backend_terra, response = "y.1", response_is_factor = TRUE, quiet = TRUE)
#
# task_spatraster = TaskClassif$new("terra", backend_terra, target = "y")
# task_rasterbrick = TaskClassif$new("raster", backend_raster, target = "y")
# task_stars = TaskClassif$new("stars", backend_stars_classif, target = "y.1")




# new
stack_classif = demo_stack_spatraster(1)
value = data.table(ID = c(0, 1), y = c("negative", "positive"))
terra::setCats(stack_classif, layer = "y", value = value)
colnames = names(stack_classif)
file = tempfile(fileext = ".tif")
terra::writeRaster(stack_classif, file)

stack_brick = demo_stack_rasterbrick(1)

tif = system.file("tif/L7_ETMs.tif", package = "stars")
l7data = stars::read_stars(tif)
colnames_stars = c("layer.1", "layer.2", "layer.3", "layer.4", "layer.5", "layer.6")
