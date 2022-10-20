test_that("DataBackendVector works", {
  vector = sf::read_sf(system.file("extdata", "leipzig_points.gpkg", package = "mlr3spatial"), stringsAsFactors = TRUE)
  primary_key = "..row_id"
  vector[[primary_key]] = seq_row(vector)
  backend = DataBackendVector$new(vector, primary_key = primary_key)

  expect_class(backend, "DataBackendVector")
  expect_class(backend$sfc, "sfc")
})

test_that("as_data_backend.sf works", {
  vector = sf::read_sf(system.file("extdata", "leipzig_points.gpkg", package = "mlr3spatial"), stringsAsFactors = TRUE)
  backend = as_data_backend(vector)

  expect_class(backend, "DataBackendVector")
  expect_class(backend$sfc, "sfc")
})
