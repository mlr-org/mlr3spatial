stack = create_stack(list(
  numeric_layer("x_1"),
  factor_layer("c_1", levels = c("a", "b"))),
dimension = 10
)

stack = add_aoi(stack)
