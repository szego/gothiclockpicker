# Expose the internal (unexported) functions so tests can exercise them directly.
make_state_codec <- gothiclockpicker:::make_state_codec
build_moves <- gothiclockpicker:::build_moves
validate_lock_inputs <- gothiclockpicker:::validate_lock_inputs
pick_lock_bfs_quick <- gothiclockpicker:::pick_lock_bfs_quick
pick_lock_bfs_full <- gothiclockpicker:::pick_lock_bfs_full
refine_full_bfs_solution <- gothiclockpicker:::refine_full_bfs_solution

small_problem <- function() {
  list(
    start = c(-3, 3, -3, 0),
    ops = list(
      c(1, 0, 0, 0),
      c(1, 1, 0, 1),
      c(0, 0, 1, 0),
      c(0, 0, 0, 1)
    )
  )
}

# The shortest path length for small_problem(); both the fast and refined paths
# must achieve it.
small_problem_solution_length <- 15

solution_path_is_valid <- function(solution, start, ops, goal, max_abs = 3) {
  states <- solution$states
  moves <- solution$moves
  if (length(states) != nrow(moves) + 1) {
    return(FALSE)
  }
  if (!isTRUE(all.equal(states[[1]], start))) {
    return(FALSE)
  }
  if (!isTRUE(all.equal(states[[length(states)]], goal))) {
    return(FALSE)
  }
  for (step in seq_len(nrow(moves))) {
    delta <- moves$sign[step] * ops[[moves$op[step]]]
    if (!isTRUE(all.equal(states[[step]] + delta, states[[step + 1]]))) {
      return(FALSE)
    }
    if (any(abs(states[[step + 1]]) > max_abs)) {
      return(FALSE)
    }
  }
  TRUE
}

count_op_switches <- function(moves) {
  if (nrow(moves) <= 1) {
    return(0)
  }
  sum(moves$op[-1] != moves$op[-nrow(moves)])
}
