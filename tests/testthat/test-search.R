test_that("pick_lock_bfs_quick returns a valid shortest path", {
  problem <- small_problem()
  goal <- rep(0, length(problem$start))
  solution <- pick_lock_bfs_quick(problem$start, problem$ops)

  expect_true(solution_path_is_valid(solution, problem$start, problem$ops, goal))
})

test_that("pick_lock_bfs_quick handles start equal to goal with no moves", {
  solution <- pick_lock_bfs_quick(c(0, 0), list(c(1, 0), c(0, 1)), goal = c(0, 0))

  expect_equal(nrow(solution$moves), 0)
  expect_length(solution$states, 1)
})

test_that("pick_lock_bfs_quick errors when the goal is unreachable", {
  # Steps of +/-2 from an odd start can never land on the even goal.
  expect_error(pick_lock_bfs_quick(1, list(2), goal = 0, max_abs = 3))
})

test_that("pick_lock_bfs_full agrees with the quick search on shortest distance", {
  problem <- small_problem()
  bfs <- pick_lock_bfs_full(problem$start, problem$ops)
  quick <- pick_lock_bfs_quick(problem$start, problem$ops)

  expect_equal(bfs$goal_distance, nrow(quick$moves))
  expect_equal(bfs$distance_of_id[bfs$start_id], 0)
})

test_that("pick_lock_bfs_full errors when the goal is unreachable", {
  expect_error(pick_lock_bfs_full(1, list(2), goal = 0, max_abs = 3))
})
