test_that("pick_lock solves the small example via the fast path", {
  problem <- small_problem()
  solution <- pick_lock(problem$start, problem$ops, refine = FALSE)

  expect_equal(nrow(solution$moves), small_problem_solution_length)
})

test_that("pick_lock solves the small example via the refined path", {
  problem <- small_problem()
  goal <- rep(0, length(problem$start))
  solution <- pick_lock(problem$start, problem$ops, refine = TRUE)

  expect_equal(nrow(solution$moves), small_problem_solution_length)
  expect_true(solution_path_is_valid(solution, problem$start, problem$ops, goal))
  expect_false(is.null(solution$switches))
})
