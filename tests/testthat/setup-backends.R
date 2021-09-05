stack_classif = demo_stack_spatraster(10)
value = data.table(ID = c(0, 1), y = c("negative", "positive"))
terra::setCats(stack_classif, layer = "y", value = value)
colnames = names(stack_classif)
file = tempfile(fileext = ".tif")
terra::writeRaster(stack_classif, file)

stack_brick = demo_stack_rasterbrick(1)

tif = system.file("tif/L7_ETMs.tif", package = "stars")
l7data = stars::read_stars(tif)
colnames_stars = c("layer.1", "layer.2", "layer.3", "layer.4", "layer.5", "layer.6")

# tasks
backend = DataBackendSpatial$new(stack_classif)
task = as_task_classif(backend, target = "y", positive = "positive")

# learners
learner = lrn("classif.rpart")
set.seed(42)
row_ids = sample(1:task$nrow, 50)
learner$train(task, row_ids = row_ids)
