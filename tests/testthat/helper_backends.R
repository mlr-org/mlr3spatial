# data -------------------------------------------------------------------------
# SpatRaster
generate_spat_raster = function() {
  stack_classif = demo_stack_spatraster(0.1)
  value = data.table(ID = c(0, 1), y = c("negative", "positive"))
  terra::set.cats(stack_classif, layer = "y", value = value)
  colnames = names(stack_classif)
  file = tempfile(fileext = ".tif")
  terra::writeRaster(stack_classif, file)
  stack_classif
}

# RasterBrick
generate_raster_brick = function() {
  demo_stack_rasterbrick(0.1)
}

# stars
generate_stars = function() {
  tif = system.file("tif/L7_ETMs.tif", package = "stars")
  stars::read_stars(tif)
}

# sf
generate_sf = function() {
  sf_data = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  sf_data$NAME = as.factor(sf_data$NAME)
  sf_data$FIPS = as.factor(sf_data$FIPS)
  sf_data
}

# backends ----------------------------------------------------------------------
# raster
generate_raster_backend = function() {
    DataBackendRaster$new(generate_spat_raster())
}

# vector
generate_vector_backend = function() {
    as_data_backend(generate_sf())
}

# tasks ------------------------------------------------------------------------
# raster
generate_raster_task = function() {
    as_task_classif(generate_raster_backend(), target = "y", positive = "positive")
}

# vector
generate_vector_task = function() {
    as_task_regr(generate_vector_backend(), target = "PERIMETER")
}
