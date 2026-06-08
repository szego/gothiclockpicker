#' Solve a lockpicking puzzle
#'
#' Find a shortest sequence of moves from `start` to `goal`. Returns a list with
#' `states` (start ... goal) and `moves` (data.frame of op + sign). Errors if no
#' path exists.
#'
#' @param start The initial state of the lock
#' @param ops The effects of moving each lock shackle
#' @param goal The desired end state of the lock (defaults to all zeros)
#' @param max_abs The maximum allowed pin location in a shackle (defaults to 3)
#' @param refine Should the solver try to simplify the result? (default `TRUE`)
#'
#' @export
pick_lock <- function(
  start,
  ops,
  goal = rep(0, length(start)),
  max_abs = 3,
  refine = TRUE
) {
  if (isTRUE(refine)) {
    return(refine_full_bfs_solution(pick_lock_bfs_full(
      start,
      ops,
      goal = goal,
      max_abs = max_abs
    )))
  }

  pick_lock_bfs_quick(
    start,
    ops,
    goal = goal,
    max_abs = max_abs
  )
}


key <- function(state) paste(state, collapse = ",")


build_moves <- function(ops) {
  num_ops <- length(ops)
  c(
    lapply(seq_len(num_ops), function(i) {
      list(op = i, sign = 1, delta = ops[[i]])
    }),
    lapply(seq_len(num_ops), function(i) {
      list(op = i, sign = -1, delta = -ops[[i]])
    })
  )
}


validate_lock_inputs <- function(start, ops, goal, max_abs) {
  num_pins <- length(start)
  stopifnot(
    length(goal) == num_pins,
    length(ops) > 0,
    all(lengths(ops) == num_pins),
    max_abs >= 0
  )
  if (any(abs(start) > max_abs)) {
    stop("initial state itself violates the max-pin-location constraint.")
  }
}


moves_to_df <- function(moves_forward) {
  data.frame(
    op = vapply(moves_forward, function(move) move$op, numeric(1)),
    sign = vapply(moves_forward, function(move) move$sign, numeric(1))
  )
}


new_solution <- function(states, moves, switches = NULL) {
  result <- list(states = states, moves = moves)
  result$switches <- switches
  structure(result, class = "solution")
}


#' Pick a lock using a quick algorithm
#'
#' This picks the lock using an early-terminating breadth-first search that
#' returns a single shortest path (`states` and `moves`).
#'
#' It's fast, but it does not explore the full set of shortest paths, so its
#' result cannot be refined; use `pick_lock_bfs_full()` for that.
#'
#' @inheritParams pick_lock
#' @noRd
pick_lock_bfs_quick <- function(
  start,
  ops,
  goal = rep(0, length(start)),
  max_abs = 3
) {
  validate_lock_inputs(start, ops, goal, max_abs)

  moves <- build_moves(ops)
  goal_key <- key(goal)

  visited <- new.env(parent = emptyenv())
  assign(
    key(start),
    list(parent = NULL, move = NULL, state = start),
    envir = visited
  )

  queue <- vector("list", 1)
  queue[[1]] <- start
  head <- 1
  tail <- 1
  found <- exists(goal_key, envir = visited, inherits = FALSE)

  while (head <= tail && !found) {
    current_state <- queue[[head]]
    head <- head + 1
    for (move in moves) {
      next_state <- current_state + move$delta
      if (any(abs(next_state) > max_abs)) {
        next
      }
      next_key <- key(next_state)
      if (exists(next_key, envir = visited, inherits = FALSE)) {
        next
      }
      assign(
        next_key,
        list(parent = key(current_state), move = move, state = next_state),
        envir = visited
      )
      tail <- tail + 1
      queue[[tail]] <- next_state
      if (next_key == goal_key) {
        found <- TRUE
        break
      }
    }
  }
  if (!found) {
    stop(
      "No path exists from initial state to target state under the constraints."
    )
  }

  states_reversed <- list()
  moves_reversed <- list()
  current_key <- goal_key
  repeat {
    node <- get(current_key, envir = visited, inherits = FALSE)
    states_reversed[[length(states_reversed) + 1]] <- node$state
    if (is.null(node$parent)) {
      break
    }
    moves_reversed[[length(moves_reversed) + 1]] <- node$move
    current_key <- node$parent
  }
  new_solution(rev(states_reversed), moves_to_df(rev(moves_reversed)))
}


#' Pick a lock and record ALL optimal paths
#'
#' This picks the lock using a beadth-first search that explores every state
#' reachable within the shortest distance from `start` to `goal`, recording each
#' state's distance.
#'
#' This is the search structure `refine_full_bfs_solution()` needs to pick a best
#' shortest path; on its own it does not return a path.
#'
#' @inheritParams pick_lock
#' @return A list capturing the explored layered graph: `state_of` and
#'   `distance_of` (environments keyed by state), `goal_distance`, `moves`,
#'   `start_key`, and `goal_key`.
#' @noRd
pick_lock_bfs_full <- function(
  start,
  ops,
  goal = rep(0, length(start)),
  max_abs = 3
) {
  validate_lock_inputs(start, ops, goal, max_abs)

  moves <- build_moves(ops)
  start_key <- key(start)
  goal_key <- key(goal)

  distance_of <- new.env(parent = emptyenv()) # state key -> distance from start
  state_of <- new.env(parent = emptyenv()) # state key -> state vector
  assign(start_key, 0, envir = distance_of)
  assign(start_key, start, envir = state_of)

  goal_distance <- if (start_key == goal_key) 0 else NA_integer_
  depth <- 0
  current_layer_keys <- start_key
  while (is.na(goal_distance) || depth < goal_distance) {
    next_layer_keys <- character(0)
    for (state_key in current_layer_keys) {
      current_state <- get(state_key, envir = state_of, inherits = FALSE)
      for (move in moves) {
        candidate <- current_state + move$delta
        if (any(abs(candidate) > max_abs)) {
          next
        }
        candidate_key <- key(candidate)
        if (exists(candidate_key, envir = distance_of, inherits = FALSE)) {
          next
        }
        assign(candidate_key, depth + 1, envir = distance_of)
        assign(candidate_key, candidate, envir = state_of)
        next_layer_keys <- c(next_layer_keys, candidate_key)
        if (candidate_key == goal_key) goal_distance <- depth + 1
      }
    }
    if (length(next_layer_keys) == 0) {
      break
    }
    depth <- depth + 1
    current_layer_keys <- next_layer_keys
  }
  if (is.na(goal_distance)) {
    stop(
      "No path exists from initial state to target state under the constraints."
    )
  }

  list(
    state_of = state_of,
    distance_of = distance_of,
    goal_distance = goal_distance,
    moves = moves,
    start_key = start_key,
    goal_key = goal_key
  )
}


#' Refine a full-BFS result to minimize operation switches
#'
#' Given the explored layered graph from `pick_lock_bfs_full()`, run a layered
#' dynamic program over the set of shortest paths to find one that uses the
#' fewest changes of shackles (op-switches), without lengthening the path.
#'
#' @param bfs The list returned by `pick_lock_bfs_full()`.
#' @return A list with `states`, `moves`, and `switches` (the minimum number of
#'   op-switches achieved).
#' @noRd
refine_full_bfs_solution <- function(bfs) {
  state_of <- bfs$state_of
  distance_of <- bfs$distance_of
  goal_distance <- bfs$goal_distance
  moves <- bfs$moves
  start_key <- bfs$start_key
  goal_key <- bfs$goal_key
  num_ops <- length(moves) %/% 2 # two moves (+/-) per operation

  no_previous_op <- 0
  best_at <- new.env(parent = emptyenv()) # augmented key -> best record
  augmented_key <- function(state_key, last_op) paste0(state_key, "|", last_op)

  start_augmented_key <- augmented_key(start_key, no_previous_op)
  assign(
    start_augmented_key,
    list(
      switches = 0,
      parent = NA_character_,
      op = NA_integer_,
      sign = NA_integer_
    ),
    envir = best_at
  )

  prev_layer_keys <- start_augmented_key
  if (goal_distance > 0) {
    for (layer in seq_len(goal_distance)) {
      next_layer_keys <- character(0)
      for (aug_key in prev_layer_keys) {
        record <- get(aug_key, envir = best_at, inherits = FALSE)
        key_parts <- strsplit(aug_key, "|", fixed = TRUE)[[1]]
        state_key <- key_parts[1]
        last_op <- as.integer(key_parts[2])
        current_state <- get(state_key, envir = state_of, inherits = FALSE)
        for (move in moves) {
          candidate <- current_state + move$delta
          candidate_key <- key(candidate)
          if (!exists(candidate_key, envir = distance_of, inherits = FALSE)) {
            next
          }
          if (
            get(candidate_key, envir = distance_of, inherits = FALSE) != layer
          ) {
            next
          }
          switch_count <- record$switches +
            (if (last_op == no_previous_op || last_op == move$op) 0 else 1)
          next_augmented_key <- augmented_key(candidate_key, move$op)
          existing <- if (
            exists(next_augmented_key, envir = best_at, inherits = FALSE)
          ) {
            get(next_augmented_key, envir = best_at, inherits = FALSE)
          } else {
            NULL
          }
          if (is.null(existing) || switch_count < existing$switches) {
            assign(
              next_augmented_key,
              list(
                switches = switch_count,
                parent = aug_key,
                op = move$op,
                sign = move$sign
              ),
              envir = best_at
            )
            if (is.null(existing)) {
              next_layer_keys <- c(next_layer_keys, next_augmented_key)
            }
          }
        }
      }
      prev_layer_keys <- next_layer_keys
    }
  }

  best_switches <- NULL
  best_goal_key <- NA_character_
  for (last_op in 0:num_ops) {
    candidate_goal_key <- augmented_key(goal_key, last_op)
    if (exists(candidate_goal_key, envir = best_at, inherits = FALSE)) {
      record <- get(candidate_goal_key, envir = best_at, inherits = FALSE)
      if (is.null(best_switches) || record$switches < best_switches) {
        best_switches <- record$switches
        best_goal_key <- candidate_goal_key
      }
    }
  }
  if (is.na(best_goal_key)) {
    stop("internal error: goal unreachable in DP layer")
  }

  states_reversed <- list()
  moves_reversed <- list()
  aug_key <- best_goal_key
  repeat {
    record <- get(aug_key, envir = best_at, inherits = FALSE)
    state_key <- strsplit(aug_key, "|", fixed = TRUE)[[1]][1]
    states_reversed[[length(states_reversed) + 1]] <- get(
      state_key,
      envir = state_of,
      inherits = FALSE
    )
    if (is.na(record$parent)) {
      break
    }
    moves_reversed[[length(moves_reversed) + 1]] <- list(
      op = record$op,
      sign = record$sign
    )
    aug_key <- record$parent
  }
  new_solution(
    rev(states_reversed),
    moves_to_df(rev(moves_reversed)),
    switches = best_switches
  )
}
