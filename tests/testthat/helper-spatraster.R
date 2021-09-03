#if (!file.exists(tempfile("demo_50mb", fileext = ".tif"))) {
#  stack = demo_stack(size = 50, layers = 5)
#  spatraster = tempfile("demo_50mb", fileext = ".tif")
#  terra::writeRaster(stack, spatraster, overwrite = TRUE)
#  rm(stack)
#}
