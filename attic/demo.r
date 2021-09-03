devtools::load_all(".")
library(terra)

# prepare raster stack
stack = demo_stack(size = 5)
# terra::writeRaster()
# stack = rast("demo_50mb.tif")

## make response a categorical raster
value = data.table(ID = c(0, 1), y = c("negative", "positive"))
setCats(stack, layer = "y", value = value)

# set task
backend = as_data_backend(stack)
task = as_task_classif(backend, target = "y")

# train model
learner = lrn("classif.svm")
learner$parallel_predict = TRUE
learner$train(task, row_ids = sample(1:task$nrow, 100))

# predict raster
future::plan("multisession", workers = 2)
predict_raster(task, learner, chunksize = 10L, filename = "target.tif")
Task


