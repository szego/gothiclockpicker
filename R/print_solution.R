#' Print a lockpicking solution
#'
#' @param x A `solution`, as returned by [pick_lock()].
#' @param ... Ignored.
#'
#' @exportS3Method base::print
print.solution <- function(x, ...) {
  states <- x$states
  moves <- x$moves
  num_steps <- nrow(moves)
  format_state <- function(state) {
    paste0("(", paste(state, collapse = ", "), ")")
  }

  cat("Solved in", num_steps, "step(s).\n\n")
  cat("Start:", format_state(states[[1]]), "\n")
  if (num_steps > 0) {
    # Collapse consecutive identical moves into a single line with a count.
    move_label <- paste(moves$op, moves$sign, sep = "/")
    move_runs <- rle(move_label)
    end_indices <- cumsum(move_runs$lengths)
    start_indices <- end_indices - move_runs$lengths + 1
    for (run_index in seq_along(move_runs$lengths)) {
      first_index <- start_indices[run_index]
      repetitions <- move_runs$lengths[run_index]
      sign_symbol <- if (moves$sign[first_index] > 0) "+" else "-"
      repetition_label <- if (repetitions > 1) {
        sprintf(" x%d", repetitions)
      } else {
        ""
      }
      cat(sprintf(
        "  %s %d%s  ->  %s\n",
        sign_symbol,
        moves$op[first_index],
        repetition_label,
        format_state(states[[end_indices[run_index] + 1]])
      ))
    }
  }
  cat("Target reached.\n")
  invisible(x)
}
