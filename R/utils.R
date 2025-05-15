`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

vlapply <- function(
  ...,
  FUN.VALUE = logical(1L) # nolint: object_name_linter.
) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

vcapply <- function(
  ...,
  FUN.VALUE = character(1L) # nolint: object_name_linter.
) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}
