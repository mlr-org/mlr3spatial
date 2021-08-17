stack_classif_terra = demo_stack_spatraster(size = 5, layers = 5)
backend_terra = DataBackendSpatRaster$new(stack_classif_terra)

stack_classif_raster = demo_stack_rasterbrick(size = 1, layers = 5)
backend_raster = DataBackendRasterBrick$new(stack_classif_raster, response = "y",
  response_is_factor = TRUE)

backend_sf = as_sf_backend(backend_raster)
sf_pred = sf::st_as_sf(raster::rasterToPoints(stack_classif_raster, spatial = TRUE))
