test_that("validate_lock_inputs rejects operations of the wrong width", {
  expect_error(validate_lock_inputs(c(0, 0), list(c(1, 0, 0)), c(0, 0), 3))
})

test_that("validate_lock_inputs rejects a start that breaks the location limit", {
  expect_error(validate_lock_inputs(c(5), list(c(1)), c(0), 3))
})
