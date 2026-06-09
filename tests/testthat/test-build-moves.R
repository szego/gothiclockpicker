test_that("build_moves pairs each operation with a +/- move whose delta matches", {
  ops <- list(c(1, 0), c(0, 2))
  moves <- build_moves(ops)

  expect_length(moves, 2 * length(ops))
  for (move in moves) {
    expect_equal(move$delta, move$sign * ops[[move$op]])
  }
  expect_setequal(
    vapply(moves, function(move) move$sign, numeric(1)),
    c(1, -1, 1, -1)
  )
})
