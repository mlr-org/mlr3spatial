with_future = function(backend, expr, ...) {
  requireNamespace("future")
  oplan = force(future::plan(backend, ...))
  on.exit(future::plan(oplan), add = TRUE)
  force(expr)
}
