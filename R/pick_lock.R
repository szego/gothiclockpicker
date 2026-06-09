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


make_state_codec <- function(num_pins, max_abs) {
  radix <- 2 * max_abs + 1
  place_values <- radix^(seq_len(num_pins) - 1)
  list(
    space_size = radix^num_pins,
    encode = function(state) sum((state + max_abs) * place_values) + 1
  )
}


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
  codec <- make_state_codec(length(start), max_abs)
  encode <- codec$encode

  seen <- logical(codec$space_size)
  parent_code <- integer(codec$space_size)
  move_of <- vector("list", codec$space_size)
  state_of <- vector("list", codec$space_size)
  start_code <- encode(start)
  goal_code <- encode(goal)
  seen[start_code] <- TRUE
  state_of[[start_code]] <- start

  queue <- start_code
  head <- 1
  tail <- 1
  found <- seen[goal_code]

  while (head <= tail && !found) {
    current_code <- queue[head]
    current_state <- state_of[[current_code]]
    head <- head + 1
    for (move in moves) {
      next_state <- current_state + move$delta
      if (any(abs(next_state) > max_abs)) {
        next
      }
      next_code <- encode(next_state)
      if (seen[next_code]) {
        next
      }
      seen[next_code] <- TRUE
      parent_code[next_code] <- current_code
      move_of[[next_code]] <- move
      state_of[[next_code]] <- next_state
      tail <- tail + 1
      queue[tail] <- next_code
      if (next_code == goal_code) {
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
  current_code <- goal_code
  repeat {
    states_reversed[[length(states_reversed) + 1]] <- state_of[[current_code]]
    if (current_code == start_code) {
      break
    }
    moves_reversed[[length(moves_reversed) + 1]] <- move_of[[current_code]]
    current_code <- parent_code[current_code]
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
#' @return A list capturing the explored layered graph: `encode` (state -> integer
#'   code), `id_of` (a dense vector mapping code -> compact state id, 0 when
#'   undiscovered), `state_of_id` and `distance_of_id` (packed, indexed by id),
#'   `goal_distance`, `moves`, `max_abs`, `num_states`, and the `start_id` and
#'   `goal_id`.
#' @noRd
pick_lock_bfs_full <- function(
  start,
  ops,
  goal = rep(0, length(start)),
  max_abs = 3
) {
  validate_lock_inputs(start, ops, goal, max_abs)

  moves <- build_moves(ops)
  codec <- make_state_codec(length(start), max_abs)
  encode <- codec$encode

  id_of <- integer(codec$space_size) # code -> compact id (0 = undiscovered)
  capacity <- 1024
  state_of_id <- vector("list", capacity)
  distance_of_id <- numeric(capacity)

  start_code <- encode(start)
  goal_code <- encode(goal)
  num_states <- 1
  id_of[start_code] <- num_states
  state_of_id[[num_states]] <- start
  distance_of_id[num_states] <- 0

  goal_distance <- if (start_code == goal_code) 0 else NA_integer_
  depth <- 0
  current_layer_ids <- num_states
  while (is.na(goal_distance) || depth < goal_distance) {
    next_layer_ids <- list()
    for (state_id in current_layer_ids) {
      current_state <- state_of_id[[state_id]]
      for (move in moves) {
        candidate <- current_state + move$delta
        if (any(abs(candidate) > max_abs)) {
          next
        }
        candidate_code <- encode(candidate)
        if (id_of[candidate_code] != 0) {
          next
        }
        num_states <- num_states + 1
        if (num_states > capacity) {
          capacity <- capacity * 2
          length(state_of_id) <- capacity
          length(distance_of_id) <- capacity
        }
        id_of[candidate_code] <- num_states
        state_of_id[[num_states]] <- candidate
        distance_of_id[num_states] <- depth + 1
        next_layer_ids[[length(next_layer_ids) + 1]] <- num_states
        if (candidate_code == goal_code) goal_distance <- depth + 1
      }
    }
    if (length(next_layer_ids) == 0) {
      break
    }
    depth <- depth + 1
    current_layer_ids <- unlist(next_layer_ids)
  }
  if (is.na(goal_distance)) {
    stop(
      "No path exists from initial state to target state under the constraints."
    )
  }

  length(state_of_id) <- num_states
  length(distance_of_id) <- num_states
  list(
    encode = encode,
    id_of = id_of,
    state_of_id = state_of_id,
    distance_of_id = distance_of_id,
    goal_distance = goal_distance,
    moves = moves,
    max_abs = max_abs,
    num_states = num_states,
    start_id = id_of[start_code],
    goal_id = id_of[goal_code]
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
  encode <- bfs$encode
  id_of <- bfs$id_of
  state_of_id <- bfs$state_of_id
  distance_of_id <- bfs$distance_of_id
  goal_distance <- bfs$goal_distance
  moves <- bfs$moves
  max_abs <- bfs$max_abs
  num_ops <- length(moves) %/% 2 # two moves (+/-) per operation

  no_previous_op <- 0
  slots_per_state <- num_ops + 1 # one slot per possible last op, plus "none"
  num_slots <- bfs$num_states * slots_per_state

  # The augmented slot already encodes (state id, last op), and the op that
  # reaches a slot is its last op, so each slot needs only these three fields.
  switches_at <- rep(NA_real_, num_slots)
  parent_at <- rep(NA_real_, num_slots)
  sign_at <- rep(NA_real_, num_slots)
  slot_of <- function(state_id, last_op) {
    (state_id - 1) * slots_per_state + last_op + 1
  }

  start_slot <- slot_of(bfs$start_id, no_previous_op)
  switches_at[start_slot] <- 0

  frontier_slots <- start_slot
  if (goal_distance > 0) {
    for (layer in seq_len(goal_distance)) {
      next_frontier_slots <- list()
      for (slot in frontier_slots) {
        state_id <- (slot - 1) %/% slots_per_state + 1
        last_op <- (slot - 1) %% slots_per_state
        current_state <- state_of_id[[state_id]]
        current_switches <- switches_at[slot]
        for (move in moves) {
          candidate <- current_state + move$delta
          if (any(abs(candidate) > max_abs)) {
            next
          }
          candidate_id <- id_of[encode(candidate)]
          if (candidate_id == 0 || distance_of_id[candidate_id] != layer) {
            next
          }
          switch_count <- current_switches +
            (if (last_op == no_previous_op || last_op == move$op) 0 else 1)
          candidate_slot <- slot_of(candidate_id, move$op)
          existing_switches <- switches_at[candidate_slot]
          if (is.na(existing_switches) || switch_count < existing_switches) {
            if (is.na(existing_switches)) {
              next_frontier_slots[[length(next_frontier_slots) + 1]] <-
                candidate_slot
            }
            switches_at[candidate_slot] <- switch_count
            parent_at[candidate_slot] <- slot
            sign_at[candidate_slot] <- move$sign
          }
        }
      }
      frontier_slots <- unlist(next_frontier_slots)
    }
  }

  best_switches <- NULL
  best_goal_slot <- NA_real_
  for (last_op in 0:num_ops) {
    goal_slot <- slot_of(bfs$goal_id, last_op)
    if (!is.na(switches_at[goal_slot])) {
      if (is.null(best_switches) || switches_at[goal_slot] < best_switches) {
        best_switches <- switches_at[goal_slot]
        best_goal_slot <- goal_slot
      }
    }
  }
  if (is.na(best_goal_slot)) {
    stop("internal error: goal unreachable in DP layer")
  }

  states_reversed <- list()
  moves_reversed <- list()
  slot <- best_goal_slot
  repeat {
    state_id <- (slot - 1) %/% slots_per_state + 1
    states_reversed[[length(states_reversed) + 1]] <- state_of_id[[state_id]]
    parent_slot <- parent_at[slot]
    if (is.na(parent_slot)) {
      break
    }
    moves_reversed[[length(moves_reversed) + 1]] <- list(
      op = (slot - 1) %% slots_per_state,
      sign = sign_at[slot]
    )
    slot <- parent_slot
  }
  new_solution(
    rev(states_reversed),
    moves_to_df(rev(moves_reversed)),
    switches = best_switches
  )
}
