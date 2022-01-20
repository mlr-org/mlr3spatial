if (requireNamespace("testthat", quietly = TRUE)) {
  library("checkmate")
  library("testthat")
  library("mlr3spatial")
  test_check("mlr3spatial")
}
