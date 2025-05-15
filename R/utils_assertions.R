#' @include utils_s7.R
NULL

assert_scopes <- function(field, scopes) {
  required_scopes <- pkg_data_get_permissions(field)
  if (!all(required_scopes %in% scopes)) err(
    class = "disallowed_scopes",
    "data derivation requires disallowed scopes: {.str {required_scopes}}"
  )
}

assert_suggests <- function(field) {
  suggests <- pkg_data_get_suggests(field)
  names(suggests) <- suggests
  has_suggests <- vlapply(suggests, requireNamespace, quietly = TRUE)

  if (!all(has_suggests)) err(
    class = "missing_suggests",
    data = list(packages = names(suggests)[!has_suggests]),
    "data derivation requires suggests: {.pkg {suggests}}"
  )
}

assert_class_is <- function(subclass, class) {
  if (!is_subclass(subclass, class)) err(
    class = "metric_not_atomic",
    "class {.cls {subclass}} is not of class {.cls {class}}"
  )
}

assert_is <- function(obj, class) {
  if (!is(obj, class)) err(
    class = "unexpected_return_class",
    "data derivation returned data of unexpected class {.cls {class(obj)}},",
    "expected {.cls {class}}"
  )
}
