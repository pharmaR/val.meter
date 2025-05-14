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

pkg_data_s3_class <- function(field_name = c()) {
  base <- "pkg_data_field"
  c(paste(base, field_name, sep = "_"), base)
}

pkg_data_class <- function(field_name = NULL) {
  S7::new_S3_class(pkg_data_s3_class(field_name))
}

as_pkg_data <- function(field_name) {
  structure(field_name, class = pkg_data_s3_class(field_name))
}

fmt <- function(..., .envir = parent.frame()) {
  cli::format_inline(..., .envir = .envir)
}

#' @param style A `list` of arguments to pass to the ellipsis arguments of
#'   [`cli::make_ansi_style()`].
cli_tag <- function(
  ...,
  scope = NULL,
  color = "blue",
  .envir = parent.frame()
) {
  symbols <- list(
    lbracket = "\U0001FB6E",
    rbracket = "\U0001FB6C",
    rhalfblock = "\u2590"
  )

  # attempt to make a bright alternative for scoped tags
  br_color <- if (is.character(color)) paste0("br_", color) else color
  tag_color <- if (is.null(scope)) color else br_color

  # convert colors to args
  color <- as.list(color)
  tag_color <- as.list(tag_color)

  # create styles for tag elements
  scope_fg <- do.call(cli::make_ansi_style, color)
  scope_bg <- do.call(cli::make_ansi_style, append(color, list(bg = TRUE)))
  tag_fg <- do.call(cli::make_ansi_style, tag_color)
  tag_bg <- do.call(cli::make_ansi_style, append(tag_color, list(bg = TRUE)))


  format_inline(
    .envir = .envir,
    scope_fg(symbols$lbracket),
    if (!is.null(scope)) {
      col_black(scope_bg(" ", scope, tag_fg(symbols$rhalfblock)))
    },
    style_bold(col_black(tag_bg(..., " "))),
    tag_fg(symbols$rbracket)
  )
}

.state <- local({
  raise <- FALSE

  raise_derive_errors <- function(value = TRUE) {
    raise <<- value
    once_on_task_callback("raise", raise <<- FALSE)
  }

  environment()
})

once_on_task_callback <- function(name, expr, envir = parent.frame()) {
  addTaskCallback(
    name = paste0(packageName(), "-clear-trace-", name),
    f = function(...) {
      eval(expr, envir = envir)
      FALSE  # don't persist after next callback
    }
  )
}

get_package_boundary_call <- function(calls = sys.calls()) {
  for (i in seq_len(sys.nframe())) {
    if (identical(parent.env(sys.frame(i)), topenv())) {
      return(sys.frame(i))
    }
  }

  sys.frame()
}

cnd_type <- function(class = NULL, cnd = "error") {
  prefix <- gsub(".", "_", packageName(), fixed = TRUE)
  c(
    sprintf("%s_%s_%s", prefix, class, cnd),
    paste(prefix, cnd, sep = "_")
  )
}

err <- function(
  ...,
  data = list(),
  class = NULL,
  .envir = parent.frame()
) {
  args <- data
  args$message <- as.character(list(...))
  args$call <- get_package_boundary_call()
  args$class <- cnd_type(class)
  args$.envir <- .envir
  do.call(cli::cli_abort, args)
}

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

as_pkg_data_derive_error <- function(x, ...) {
  after <- match(cnd_type(), class(x), nomatch = 1L) - 1L
  class(x) <- append(class(x), cnd_type("derive")[[1L]], after = after)
  new_data <- list(...)
  x[names(new_data)] <- new_data
  x
}

impl_data_derive <- function(name, fn, resource) {
  return_class <- pkg_data_get_class(name)
  method(pkg_data_derive, list(pkg_data_class(name), new_pkg, resource)) <-
    function(field, pkg, resource, ...) {
      assert_scopes(field, pkg@scopes)
      assert_suggests(field)
      convert(fn(field, pkg, resource, ...), return_class)
    }
}

impl_data_meta <- function(
  name,
  class = class_any,
  description = "",
  tags = pkg_data_tags(c()),
  permissions = pkg_data_permissions(FALSE),
  suggests = character(0L)
) {
  tags <- convert(tags, pkg_data_tags)
  description <- convert(description, class_character)
  permissions <- convert(permissions, pkg_data_permissions)

  if (is.character(class)) class <- S7::new_S3_class(class)
  class <- S7::as_class(class)

  method(pkg_data_get_permissions, pkg_data_class(name)) <-
    function(field) permissions

  method(pkg_data_get_description, pkg_data_class(name)) <-
    function(field) description

  method(pkg_data_get_tags, pkg_data_class(name)) <-
    function(field) tags

  method(pkg_data_get_suggests, pkg_data_class(name)) <-
    function(field) suggests

  method(pkg_data_get_class, pkg_data_class(name)) <-
    function(field) class
}

impl_data <- function(
  name,
  fn,
  for_resource = pkg_resource,
  ...,
  metric = FALSE
) {
  if (...length() > 0L)
    impl_data_meta(name = name, ...)

  if (!missing(fn))
    impl_data_derive(name = name, fn = fn, resource = for_resource)

  if (metric)
    impl_metric(name)
}

impl_metric <- function(name, class = pkg_data_get_class(name)) {
  assert_class_is(class, class_atomic)
  method(pkg_data_is_metric, pkg_data_class(name)) <-
    function(field) TRUE
}
