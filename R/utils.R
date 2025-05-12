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

glue <- function(..., .envir = parent.frame()) {
  cli::cli_fmt(cli::cli_text(..., .envir = .envir))
}

.desc <- local({
  desc <- NULL

  load <- function(lib) {
    if (!is.null(desc)) return(desc)
    dcf_path <- file.path(lib, packageName(), "DESCRIPTION")
    dcf <- read.dcf(dcf_path, keep.white = TRUE)
    desc <<- as.list(as.data.frame(dcf))
  }

  features <- function() {
    is_feat <- startsWith(names(desc), "Config/Feature")
    feats <- desc[is_feat]
    names(feats) <- gsub("^Config/Feature/|@R$", "", names(desc)[is_feat])
    lapply(feats, function(x) eval(parse(text = x), envir = baseenv()))
  }

  environment()
})

.trace <- local({
  raise <- FALSE

  raise_derive_errors <- function(value = TRUE) {
    raise <<- value
    set_on_task_callback("raise", raise <<- FALSE)
  }

  set_on_task_callback <- function(name, value, envir = parent.frame()) {
    addTaskCallback(
      name = paste0(packageName(), "-clear-trace-", name),
      function(...) {
        eval(expr, envir = envir)
        FALSE  # don't persist after next callback
      }
    )
  }

  environment()
})

get_package_boundary_call <- function(calls = sys.calls()) {
  for (i in seq_len(sys.nframe())) {
    if (identical(parent.env(sys.frame(i)), topenv())) {
      return(sys.frame(i))
    }
  }

  sys.frame()
}

err_type <- function(class = NULL) {
  prefix <- gsub(".", "_", packageName(), fixed = TRUE)
  suffix <- "error"
  c(
    sprintf("%s_%s_%s", prefix, class, suffix),
    paste(prefix, suffix, sep = "_")
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
  args$class <- err_type(class)
  args$.envir <- .envir
  do.call(cli::cli_abort, args)
}

assert_scopes <- function(field, scopes) {
  required_scopes <- pkg_data_get_scopes(field)
  if (!all(required_scopes %in% scopes)) err(
    class = err_type("disallowed_scopes"),
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
    class = err_type("metric_data_not_atomic"),
    "class {.cls {subclass}} is not of class {.cls {class}}"
  )
}

assert_is <- function(obj, class) {
  if (!is(obj, class)) err(
    class = err_type("unexpected_return_class"),
    "data derivation returned data of unexpected class {.cls {class(obj)}},",
    "expected {.cls {class}}"
  )
}

as_pkg_data_derive_error <- function(x, ...) {
  after <- match(err_type(), class(x), nomatch = 1L) - 1L
  class(x) <- append(class(x), err_type("derive")[[1L]], after = after)
  new_data <- list(...)
  x[names(new_data)] <- new_data
  x
}

register_data <- function(
  name,
  class = class_any,
  ...,
  description = "",
  tags = pkg_data_tags(),
  scopes = pkg_data_scopes(),
  suggests = character(0L)
) {
  dots <- list(...)
  tags <- convert(tags, pkg_data_tags)
  description <- convert(description, class_character)
  scopes <- convert(scopes, pkg_data_scopes)

  if (is.character(class)) class <- S7::new_S3_class(class)
  return_class <- S7::as_class(class)

  if (length(dots) < 1L) {
    stop("At least one `derive` method must be provided")
  }

  # special case, when a single function is provided assume it applies to any
  # resource type, pkg_resource
  if (length(dots) == 1L && is.null(names(dots))) {
    names(dots) <- "pkg_resource"
  }

  # if any methods are unnamed, error and enforce naming convention
  if (is.null(names(dots)) || any(names(dots) == "")) {
    stop("All methods must be named with a valid `pkg_resource` class name")
  }

  method(pkg_data_get_scopes, pkg_data_class(name)) <-
    function(field) scopes

  method(pkg_data_get_description, pkg_data_class(name)) <-
    function(field) description

  method(pkg_data_get_tags, pkg_data_class(name)) <-
    function(field) tags

  method(pkg_data_get_suggests, pkg_data_class(name)) <-
    function(field) suggests

  for (i in seq_along(dots)) {
    resource_class <- eval(parse(text = names(dots)[[i]]))
    fn <- dots[[i]]

    fn_sig <- list(
      params = list(pkg_data_class(name), pkg, resource_class),
      return = class
    )

    names(fn_sig$params) <-
      utils::head(names(formals(fn)), length(fn_sig$params))

    method(pkg_data_get_derive_signature, pkg_data_class(name)) <-
      function(field) fn_sig

    method(pkg_data_derive, fn_sig$params) <-
      function(field, pkg, resource, scopes = opt("scopes"), ...) {
        .trace$raise_derive_errors()
        on.exit(.trace$raise_derive_errors(FALSE))

        tryCatch(
          {
            assert_scopes(field, scopes)
            assert_suggests(field)
            convert(fn(field, pkg, resource, ...), return_class)
          },
          val_meter_error = function(e) {
            as_pkg_data_derive_error(e, field = as.character(field))
          }
        )
      }
  }
}

register_metric <- function(
  name,
  class,
  ...,
  tags = pkg_data_tags()
) {
  assert_class_is(class, class_atomic)

  method(pkg_data_is_metric, pkg_data_class(name)) <-
    function(field) TRUE

  register_data(name = name, class = class, tags = tags, ...)
}
