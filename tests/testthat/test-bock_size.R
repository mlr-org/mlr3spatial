test_that("chunk size is 1 out of 8 rows", {
  raster = create_stack(list(
    numeric_layer("x_1")),
  dimension = 8)

  bs = block_size(raster, chunksize = 64 * 1e-6)
  expect_equal(bs$cells_seq, cumsum(c(1, rep(8, 7))))
  expect_equal(bs$cells_to_read, rep(8, 8))
})

test_that("chunk size is 2 out of 8 rows", {
  raster = create_stack(list(
    numeric_layer("x_1")),
  dimension = 8)

  bs = block_size(raster, chunksize = 64 * 2 * 1e-6)
  expect_equal(bs$cells_seq, cumsum(c(1, rep(16, 3))))
  expect_equal(bs$cells_to_read, rep(16, 4))
})

test_that("chunk size is 1/2 out of 8 rows", {
  raster = create_stack(list(
    numeric_layer("x_1")),
  dimension = 8)

  bs = block_size(raster, chunksize = 32 * 1e-6) # chunk size is round up to 1 rows
  expect_equal(bs$cells_seq, cumsum(c(1, rep(8, 7))))
  expect_equal(bs$cells_to_read, rep(8, 8))
})

test_that("chunk size is 1 1/2 out of 8 rows", {
  raster = create_stack(list(
    numeric_layer("x_1")),
  dimension = 8)

  bs = block_size(raster, chunksize = 96 * 1e-6) # chunk size is round up to 2 rows
  expect_equal(bs$cells_seq, cumsum(c(1, rep(16, 3))))
  expect_equal(bs$cells_to_read, rep(16, 4))
})

test_that("chunk size is 3 rows out of 8 rows", {
  raster = create_stack(list(
    numeric_layer("x_1")),
  dimension = 8)

  bs = block_size(raster, chunksize = 64 * 3 * 1e-6)
  expect_equal(bs$cells_seq, cumsum(c(1, rep(24, 2))))
  expect_equal(bs$cells_to_read, c(24, 24, 16))
})

test_that("chunk size is 8 out of 8 rows", {
  raster = create_stack(list(
    numeric_layer("x_1")),
  dimension = 8)

  bs = block_size(raster, chunksize = 64 * 8 * 1e-6)
  expect_equal(bs$cells_seq, 1)
  expect_equal(bs$cells_to_read, 64)
})

test_that("chunk size is 9 out of 8 rows", {
  raster = create_stack(list(
    numeric_layer("x_1")),
  dimension = 8)

  bs = block_size(raster, chunksize = 64 * 9 * 1e-6)
  expect_equal(bs$cells_seq, 1)
  expect_equal(bs$cells_to_read, 64)
})

test_that("chunk size is 16 out of 8 rows", {
  raster = create_stack(list(
    numeric_layer("x_1")),
  dimension = 8)

  bs = block_size(raster, chunksize = 64 * 16 * 1e-6)
  expect_equal(bs$cells_seq, 1)
  expect_equal(bs$cells_to_read, 64)
})
