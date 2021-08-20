stack_classif_terra = demo_stack_spatraster(size = 5, layers = 5)
backend_terra = DataBackendSpatRaster$new(stack_classif_terra)

stack_classif_raster = demo_stack_rasterbrick(size = 1, layers = 5)
backend_raster = DataBackendRasterBrick$new(stack_classif_raster, response = "y",
  response_is_factor = TRUE)

backend_sf = as_sf_backend(backend_raster)
sf_pred = sf::st_as_sf(raster::rasterToPoints(stack_classif_raster, spatial = TRUE))

tif = system.file("tif/L7_ETMs.tif", package = "stars")
l7data = stars::read_stars(tif)
# l7data[ ,c('x', 'y')] <- list(NULL)
backend_stars = as_stars_backend(l7data, quiet = TRUE)
