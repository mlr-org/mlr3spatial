#' @title Predict on Spatial Objects with mlr3 Learners
#'
#' @description
#' This function allows to directly predict mlr3 learners on various spatial objects.
#'
#' @param newdata ([terra::SpatRaster] | `stars::stars` | [sf::sf] | `raster::RasterStack` | `raster::RasterBrick`).
#'   New data to predict on.
#'   All spatial data formats convertible by `as_data_backend()` are supported e.g. [terra::SpatRaster] or [sf::sf].
#' @param learner ([mlr3::Learner]).
#'   Learner with trained model.
#' @template param-chunksize
#' @param format (`character(1)`)\cr
#'   Output class of the resulting object.
#'   Accepted values are `"raster"`, `"stars"` and `"terra"` if the input is a raster.
#'   Note that when choosing something else than `"terra"`,
#'   the spatial object is converted into the respective format
#'   which might cause overhead both in runtime and memory allocation.
#'   For vector data only `"sf"` is supported.
#' @param filename (`character(1)`)\cr
#'   Path where the spatial object should be written to.
#'
#' @details
#' The type of the prediction is taken from the `$predict_type` of the `learner`.
#' For classification learners with `predict_type = "prob"`,
#' the probability of each class is returned e.g. as a raster layer per class or as a column per class in an sf object.
#'
#' @return Spatial object of class given in argument `format`.
#' @examples
#' \dontshow{data.table::setDTthreads(1)}
#' library(terra, exclude = "resample")
#'
#' # fit rpart on training points
#' task_train = tsk("leipzig")
#' learner = lrn("classif.rpart")
#' learner$train(task_train)
#'
#' # load raster
#' stack = rast(system.file("extdata", "leipzig_raster.tif", package = "mlr3spatial"))
#'
#' # predict land cover classes
#' pred = predict_spatial(stack, learner, chunksize = 1L)
#'
#' # predict land cover probabilities
#' learner = lrn("classif.rpart", predict_type = "prob")
#' learner$train(task_train)
#' pred = predict_spatial(stack, learner, chunksize = 1L)
#' @export
predict_spatial = function(newdata, learner, chunksize = 200L, format = "terra", filename = NULL) {
  task = as_task_unsupervised(newdata)
  assert_multi_class(task$backend, c("DataBackendRaster", "DataBackendVector"))
  assert_learner(learner)

  if (test_class(task$backend, "DataBackendRaster")) {
    assert_number(chunksize)
    assert_choice(format, c("terra", "raster", "stars"))
    filename = filename %??% tempfile(fileext = ".tif")
    assert_path_for_output(filename)

    stack = task$backend$stack
    start_time = proc.time()[3]
    learner = switch(
      learner$task_type,
      "classif" = LearnerClassifSpatial$new(learner),
      "regr" = LearnerRegrSpatial$new(learner)
    )

    predict_prob = learner$predict_type == "prob"
    levels = if (learner$task_type == "classif") {
      learner$learner$state$train_task$levels()[[learner$learner$state$train_task$target_names]]
    }

    # calculate block size
    bs = block_size(stack, chunksize)

    # initialize target raster with one layer per class when predicting probabilities
    target_raster = terra::rast(
      terra::ext(stack),
      resolution = terra::res(stack),
      crs = terra::crs(stack),
      nlyrs = if (predict_prob) length(levels) else 1L
    )
    terra::writeStart(target_raster, filename = filename, overwrite = TRUE, datatype = "FLT8S")

    lg$info("Start raster prediction")
    lg$info(
      "Prediction is executed with a chunksize of %s Megabytes, %i chunk(s) in total, %i values per chunk",
      as.character(chunksize),
      length(bs$cells_seq),
      ceiling(terra::ncell(task$backend$stack) / length(bs$cells_seq))
    )

    mlr3misc::pmap(
      list(bs$cells_seq, bs$cells_to_read, seq_along(bs$cells_seq)),
      function(cells_seq, cells_to_read, n) {
        stack = task$backend$stack
        pred = learner$predict(task, row_ids = cells_seq:((cells_seq + cells_to_read - 1)))
        terra::writeValues(
          x = target_raster,
          v = if (predict_prob) pred$prob[, levels, drop = FALSE] else pred$response,
          start = terra::rowFromCell(stack, cells_seq), # start row number
          nrows = terra::rowFromCell(stack, cells_to_read)
        ) # how many rows
        lg$info("Chunk %i of %i finished", n, length(bs$cells_seq))
      }
    )

    terra::writeStop(target_raster)
    lg$info("Finished raster prediction in %i seconds", as.integer(proc.time()[3] - start_time))

    if (learner$task_type == "classif" && !predict_prob) {
      value = data.table(ID = seq_along(levels), categories = levels)
      target_raster = terra::categories(target_raster, value = value)
    }
    target_raster = set_names(
      target_raster,
      if (predict_prob) levels else learner$learner$state$train_task$target_names
    )

    switch(
      format,
      "terra" = target_raster,
      "stars" = stars::st_as_stars(target_raster),
      "raster" = as(target_raster, "Raster")
    )
  } else {
    assert_string(format, "sf")
    if (!is.null(filename)) {
      assert_path_for_output(filename)
    }
    pred = learner$predict_newdata(task$data())
    vector = if (learner$predict_type == "prob") {
      set_names(
        sf::st_as_sf(data.frame(pred$prob, task$backend$sfc)),
        c(colnames(pred$prob), "geometry")
      )
    } else {
      set_names(
        sf::st_as_sf(data.frame(pred$response, task$backend$sfc)),
        c(learner$state$train_task$target_names, "geometry")
      )
    }

    if (!is.null(filename)) {
      sf::st_write(vector, filename, quiet = TRUE)
    }
    vector
  }
}
