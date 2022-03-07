set.seed(42)
# data -------------------------------------------------------------------------
# SpatRaster
stack_classif = demo_stack_spatraster(0.1)
value = data.table(ID = c(0, 1), y = c("negative", "positive"))
terra::set.cats(stack_classif, layer = "y", value = value)
colnames = names(stack_classif)
file = tempfile(fileext = ".tif")
terra::writeRaster(stack_classif, file)

# RasterBrick
stack_brick = demo_stack_rasterbrick(0.1)

# stars
tif = system.file("tif/L7_ETMs.tif", package = "stars")
l7data = stars::read_stars(tif)
colnames_stars = c("band1", "band2", "band3", "band4", "band5", "band6")

# sf
sf_data = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
sf_data$NAME = as.factor(sf_data$NAME)
sf_data$FIPS = as.factor(sf_data$FIPS)

# backends ----------------------------------------------------------------------
backend = DataBackendRaster$new(stack_classif)
backend_vec = as_data_backend(sf_data)

# tasks ------------------------------------------------------------------------
task = as_task_classif(backend, target = "y", positive = "positive")
task_vec = as_task_regr(backend_vec, target = "PERIMETER")

# learners ---------------------------------------------------------------------
learner = lrn("classif.rpart")
learner_regr = lrn("regr.rpart")
row_ids = sample(1:task$nrow, 50)
row_ids_vec = sample(1:task_vec$nrow, 50)

learner$train(task, row_ids = row_ids)
learner_regr$train(task_vec, row_ids = row_ids_vec)
