#' @include utils_s7.R
#' @include utils_err.R
NULL

assert_permissions <- function(required_permissions, provided_permissions) {
  if (!all(required_permissions %in% provided_permissions)) {
    err$disallowed_permissions(permissions = required_permissions)
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
