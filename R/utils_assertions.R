#' @include utils_s7.R
#' @include utils_err.R
NULL

assert_scopes <- function(field, scopes) {
  required_scopes <- pkg_data_get_permissions(field)
  if (!all(required_scopes %in% scopes))
    err$disallowed_scopes(scopes = required_scopes)
}

assert_suggests <- function(field) {
  suggests <- pkg_data_get_suggests(field)
  names(suggests) <- suggests
  has_suggests <- vlapply(suggests, requireNamespace, quietly = TRUE)
  if (!all(has_suggests))
    err$missing_suggests(packages = names(suggests)[!has_suggests])
}

assert_metric_is_atomic <- function(class) {
  if (!is_subclass(class, class_atomic))
    err$metric_not_atomic()
}
