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

viapply <- function(
  ...,
  FUN.VALUE = integer(1L) # nolint: object_name_linter.
) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}


vnapply <- function(
  ...,
  FUN.VALUE = numeric(1L) # nolint: object_name_linter.
) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

tocamel <- function(x, ...) {
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}

rversion <- function() {
  numeric_version(paste(collapse = ".", R.version[c("major", "minor")]))
}
