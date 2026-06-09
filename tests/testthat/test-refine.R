test_that("refine_full_bfs_solution returns a valid path of the optimal length", {
  problem <- small_problem()
  goal <- rep(0, length(problem$start))
  bfs <- pick_lock_bfs_full(problem$start, problem$ops)
  solution <- refine_full_bfs_solution(bfs)

  expect_true(solution_path_is_valid(
    solution,
    problem$start,
    problem$ops,
    goal
  ))
  expect_equal(nrow(solution$moves), bfs$goal_distance)
})

test_that("refine_full_bfs_solution finds the known minimum number of switches", {
  # Reaching (0, 0) from (-1, -1) needs each of the two operations exactly once,
  # so any shortest path switches operations exactly once.
  start <- c(-1, -1)
  ops <- list(c(1, 0), c(0, 1))
  bfs <- pick_lock_bfs_full(start, ops)
  solution <- refine_full_bfs_solution(bfs)

  expect_equal(solution$switches, 1)
})

test_that("refine_full_bfs_solution improves on an arbitrary shortest path", {
  start <- c(1, 0, -2)
  ops <- list(c(-1, -1, -1), c(1, 0, 1), c(-1, -1, 0))
  goal <- rep(0, length(start))
  bfs <- pick_lock_bfs_full(start, ops, max_abs = 2)
  refined <- refine_full_bfs_solution(bfs)
  quick <- pick_lock_bfs_quick(start, ops, max_abs = 2)

  expect_true(solution_path_is_valid(refined, start, ops, goal, max_abs = 2))
  expect_equal(nrow(refined$moves), bfs$goal_distance)
  expect_equal(refined$switches, count_op_switches(refined$moves))
  expect_lt(refined$switches, count_op_switches(quick$moves))
})

test_that("refine_full_bfs_solution handles start equal to goal with no moves", {
  bfs <- pick_lock_bfs_full(c(0, 0), list(c(1, 0), c(0, 1)), goal = c(0, 0))
  solution <- refine_full_bfs_solution(bfs)

  expect_equal(nrow(solution$moves), 0)
  expect_length(solution$states, 1)
  expect_equal(solution$switches, 0)
})
