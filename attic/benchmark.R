library(mlr3spatial)
library(mlr3learners)
library(tictoc)

# 10 MB - in memory ------------------------------------------------------------

# train task
stack = create_stack(list(
  numeric_layer("x_1"),
  factor_layer("y", levels = c("a", "b"))),
layer_size = 10)
vector = sample_stack(stack, n = 100)
task_train = as_task_classif(vector, id = "test_vector", target = "y")
learner = lrn("classif.ranger")
learner$train(task_train)

# predict task
dt = as.data.table(terra::values(stack))
dt[, y := factor(y, levels = c("a", "b"))]
backend = as_data_backend(dt)
task = as_task_classif(dt, id = "test", target = "y")

tic()
pred = learner$predict(task) # 74 seconds
toc()


# 10 MB - terra ----------------------------------------------------------------

# train task
stack = create_stack(list(
  numeric_layer("x_1"),
  factor_layer("y", levels = c("a", "b"))),
layer_size = 10)
vector = sample_stack(stack, n = 100)
task_train = as_task_classif(vector, id = "test_vector", target = "y")
learner = lrn("classif.ranger")
learner$train(task_train)

# predict task
stack$y = NULL

predfun = function(model, data) {
  library(mlr3)
  library(mlr3learners)

  model$predict_newdata(data)$response
}

tic()
terra::predict(stack, learner, fun = predfun) # 78 seconds
toc()

tic()
terra::predict(stack, learner, fun = predfun, cores = 4) # 36 seconds
toc()

tic()
terra::predict(stack, learner, fun = predfun, cores = 8) # 47 seconds
toc()


# 10 MB - 1 chunk --------------------------------------------------------------

# train task
stack = create_stack(list(
  numeric_layer("x_1"),
  factor_layer("y", levels = c("a", "b"))),
layer_size = 10)
vector = sample_stack(stack, n = 100)
task_train = as_task_classif(vector, id = "test_vector", target = "y")
learner = lrn("classif.ranger")
learner$train(task_train)

# predict task
stack$y = NULL
backend = DataBackendRaster$new(stack, task_train)
task_predict = as_task_classif(backend, id = "test", target = "y")
learner$parallel_predict = TRUE

ras = predict_spatial(task_predict, learner, chunksize = 10L) # 71 seconds

with_future("multicore", workers = 2, {
  ras = predict_spatial(task_predict, learner, chunksize = 10L) # 38 seconds
})

with_future("multicore", workers = 4, {
  ras = predict_spatial(task_predict, learner, chunksize = 10L) # 21 seconds
})

with_future("multicore", workers = 8, {
  ras = predict_spatial(task_predict, learner, chunksize = 10L) # 19 seconds
})

# 50 MB - 5 chunks -------------------------------------------------------------

# train task
stack = create_stack(list(
  numeric_layer("x_1"),
  factor_layer("y", levels = c("a", "b"))),
layer_size = 50)
vector = sample_stack(stack, n = 100)
task_train = as_task_classif(vector, id = "test_vector", target = "y")
learner = lrn("classif.ranger")
learner$train(task_train)

# predict task
stack$y = NULL
backend = DataBackendRaster$new(stack, task_train)
task_predict = as_task_classif(backend, id = "test", target = "y")
learner$parallel_predict = TRUE

ras = predict_spatial(task_predict, learner, chunksize = 10L) # 386 seconds

with_future("multicore", workers = 2, {
  ras = predict_spatial(task_predict, learner, chunksize = 10L) #
})

with_future("multicore", workers = 4, {
  ras = predict_spatial(task_predict, learner, chunksize = 10L) # 142 seconds
})

with_future("multicore", workers = 8, {
  ras = predict_spatial(task_predict, learner, chunksize = 10L) #
})

# 20 MB - 1 chunk --------------------------------------------------------------

# train task
stack = create_stack(list(
  numeric_layer("x_1"),
  factor_layer("y", levels = c("a", "b"))),
layer_size = 20)
vector = sample_stack(stack, n = 100)
task_train = as_task_classif(vector, id = "test_vector", target = "y")
learner = lrn("classif.ranger")
learner$train(task_train)

# predict task
stack$y = NULL
backend = DataBackendRaster$new(stack, task_train)
task_predict = as_task_classif(backend, id = "test", target = "y")
learner$parallel_predict = TRUE

ras = predict_spatial(task_predict, learner, chunksize = 20L) # 221 seconds

with_future("multicore", workers = 2, {
  ras = predict_spatial(task_predict, learner, chunksize = 10L) #
})

with_future("multicore", workers = 4, {
  ras = predict_spatial(task_predict, learner, chunksize = 20L) # 61 seconds
})

with_future("multicore", workers = 8, {
  ras = predict_spatial(task_predict, learner, chunksize = 10L) #
})
