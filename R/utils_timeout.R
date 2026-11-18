#' Time limits for package data derivation
#'
#' Data derivations are given a deadline, calculated from the `timeout` and
#' `deadline` options and any deadline of an enclosing derivation. Derivations
#' are interrupted using [`setTimeLimit()`] when the deadline is reached, and
#' derivations that start after their deadline fail immediately. In both cases,
#' a `timeout` error is recorded in place of the data.
#'
#' @note [`setTimeLimit()`] is only checked while R code is running. Blocking
#'   calls into other processes (such as [`system()`] or [`system2()`]) can't
#'   be interrupted and are only stopped once they return.
#'
#' @keywords internal
#' @name timeout
NULL

#' @describeIn timeout
#' Deadline of the derivation currently being evaluated, as seconds since the
#' epoch.
.timeout_state <- local({
  deadline <- Inf
  environment()
})

#' @describeIn timeout
#' Normalize a deadline option value to seconds since the epoch.
as_deadline <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(Inf)
  }

  if (!(inherits(x, "POSIXt") || is.numeric(x)) || length(x) != 1L) {
    stop(
      "deadline must be a single POSIXct date-time or a number of seconds ",
      "since the epoch"
    )
  }

  as.numeric(as.POSIXct(x))
}

#' @describeIn timeout
#' Calculate the deadline for a new derivation.
derive_deadline <- function(
  timeout = opt("timeout"),
  deadline = opt("deadline"),
  outer = .timeout_state$deadline,
  now = as.numeric(Sys.time())
) {
  timeout <- if (is.null(timeout) || is.na(timeout)) Inf else timeout
  min(now + timeout, as_deadline(deadline), outer)
}

#' @describeIn timeout
#' Test whether a deadline has passed.
deadline_passed <- function(deadline, now = as.numeric(Sys.time())) {
  is.finite(deadline) && now >= deadline
}

#' @describeIn timeout
#' Evaluate an expression with an elapsed time limit, signalling an error if
#' the deadline passes before or during its evaluation. Restores the time limit
#' of an enclosing derivation on exit.
with_deadline <- function(deadline, expr) {
  if (!is.finite(deadline)) {
    return(expr)
  }

  if (deadline_passed(deadline)) {
    stop("deadline passed before derivation started")
  }

  outer <- .timeout_state$deadline
  .timeout_state$deadline <- deadline
  on.exit(
    {
      .timeout_state$deadline <- outer
      set_elapsed_time_limit(outer)
    },
    add = TRUE
  )

  set_elapsed_time_limit(deadline)
  result <- expr

  # derivations may intercept the time limit error and return a value anyway,
  # so treat any result produced after the deadline as having timed out
  if (deadline_passed(deadline)) {
    stop("derivation finished after its deadline")
  }

  result
}

#' @describeIn timeout
#' Set the session's elapsed time limit to expire at `deadline`.
set_elapsed_time_limit <- function(deadline) {
  if (!is.finite(deadline)) {
    setTimeLimit(elapsed = Inf)
    return(invisible())
  }

  # an expired outer deadline should interrupt promptly once resumed
  remaining <- max(deadline - as.numeric(Sys.time()), 0.01)
  setTimeLimit(elapsed = remaining, transient = TRUE)
  invisible()
}
