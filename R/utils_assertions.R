#' @include utils_s7.R
#' @include utils_err.R
NULL

assert_scopes <- function(required_scopes, provided_scopes) {
  if (!all(required_scopes %in% provided_scopes)) {
    err$disallowed_scopes(scopes = required_scopes)
  }
}

assert_suggests <- function(required_suggests) {
  names(required_suggests) <- required_suggests
  has_suggests <- vlapply(required_suggests, requireNamespace, quietly = TRUE)
  if (!all(has_suggests)) {
    err$missing_suggests(suggests = names(required_suggests)[!has_suggests])
  }
}

assert_metric_is_atomic <- function(class) {
  if (!is_subclass(class, class_atomic)) {
    err$metric_not_atomic()
  }
}
