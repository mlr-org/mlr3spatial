library(mlr3)
library(mlr3raster)
library(mlr3learners)
library(raster)
library(data.table)
library(tictoc)

# Create demo stack
stack = demo_stack()
writeRaster(stack, "inst/demo_stack_500mb.tif", overwrite = TRUE)
rm(stack)

# Build model
data = as.data.table(sampleRandom(stack("inst/demo_stack_500mb.tif"), 500))
names(data) = paste0("var", 1:5)
data = data[, var1:=as.factor(var1)]

task = TaskClassif$new(id = "raster", backend = data, target = "var1", positive = "1")

learner_svm = LearnerClassifSVMParallel$new()
learner_svm$train(task, row_ids = 1:250)
prediction = learner_svm$predict(task, row_ids = 251:500)
prediction$score(msr("classif.acc"))

# Prediction raster
data_stack = stack("inst/demo_stack_500mb.tif")
names(data_stack) = paste0("var", 1:5)
data_stack = dropLayer(data_stack, 1)

# Benchmark
## Sequential execution
future::plan("sequential")

reclassify_table = data.table(task = c(0,1), raster = c(10, 11))
pred = PredictionRasterClassif$new(data_stack, task, reclassify_table)
pred$chunksize = 300
tic()
ras = pred$predict(learner_svm)
toc()

## Parallel execution
future::plan("multisession")

pred = PredictionRasterClassif$new(data_stack, task, reclassify_table)
pred$chunksize = 300
tic()
ras = pred$predict(learner_svm)
toc()

# Parallel package intern execution
learner_ranger = LearnerClassifRanger$new()
learner_ranger$train(task, row_ids = 1:250)
prediction = learner_ranger$predict(task, row_ids = 251:500)
prediction$score(msr("classif.acc"))

pred = PredictionRasterClassif$new(data_stack, task)
pred$chunksize = 100
tic()
ras = pred$predict(learner_ranger)
toc()
