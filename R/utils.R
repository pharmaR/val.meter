vlapply <- function(
  ...,
  FUN.VALUE = logical(1L) # nolint: object_name_linter.
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
  structure(list(), class = pkg_data_s3_class(field_name))
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
  bottom <- NULL

  set_bottom <- function(n = 0L) {
    bottom <<- sys.frames()[[sys.nframe() - n - 1L]]
    clear_bottom_on_task_callback()
  }

  clear_bottom_on_task_callback <- function() {
    addTaskCallback(
      function(...) {
        bottom <<- NULL
        FALSE # don't persist after next callback
      },
      name = paste0(packageName(), "-clear-trace-bottom")
    )
  }

  environment()
})

get_generic_call <- function(calls) {
  for (i in rev(seq_along(calls))) {
    if (identical(calls[[i]], quote(S7::S7_dispatch()))) {
      return(calls[[i - 1L]])
    }
  }

  calls[[length(calls)]]
}

err_type <- function(class) {
  prefix <- gsub(".", "_", packageName(), fixed = TRUE)
  c(sprintf("%s_%s", prefix, class), prefix)
}

err <- function(msg, class = NULL) {
  errorCondition(msg, class = err_type(class))
}

assert_scopes <- function(field, scopes) {
  required_scopes <- pkg_data_get_scopes(field)
  if (!all(required_scopes %in% scopes)) {
    call <- get_generic_call(sys.calls())
    msg <- glue(
      "data derivation requires disallowed scopes: ",
      "{.str {required_scopes}}"
    )

    cli::cli_abort(
      call = call,
      msg,
      err_type("disallowed_scopes"),
      .frame = .trace$bottom
    )
  }
}

assert_suggests <- function(field) {
  suggests <- pkg_data_get_suggests(field)
  names(suggests) <- suggests

  has_suggests <- vlapply(suggests, requireNamespace, quietly = TRUE)

  if (!all(has_suggests)) {
    call <- get_generic_call(sys.calls())
    msg <- glue("data derivation requires suggests: ", "{.pkg {suggests}}")

    cli::cli_abort(
      call = call,
      msg,
      err_type("missing_suggests"),
      .frame = .trace$bottom
    )
  }
}

assert_class_is <- function(subclass, class) {
  if (!is_subclass(subclass, class)) {
    call <- get_generic_call(sys.calls())
    msg <- "test"
    # msg <- glue("class {.cls {subclass}} is not a subclass of {.cls {class}}")

    cli::cli_abort(
      call = call,
      msg,
      err_type("metric_data_not_atomic")
    )
  }
}

assert_is <- function(obj, class) {
  if (!is(obj, class)) {
    call <- get_generic_call(sys.calls())
    msg <- glue("data derivation returned data of unexpected class {.cls {class(obj)}}, expected {.cls {class}}")

    cli::cli_abort(
      call = call,
      msg,
      err_type("unexpected_return_class"),
      .frame = .trace$bottom
    )
  }
}

register_data <- function(
  name,
  class = class_any,
  ...,
  description = "",
  tags = pkg_data_tags(),
  scopes = pkg_data_scopes(),
  suggests = c()
) {
  dots <- list(...)
  tags <- convert(tags, pkg_data_tags)
  description <- convert(description, class_character)
  scopes <- convert(scopes, pkg_data_scopes)

  return_class <- class
  return_class_or_derive_error <- S7::new_union(class, pkg_data_derive_error)

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

  for (i in seq_along(dots)) {
    resource_class <- eval(parse(text = names(dots)[[i]]))
    fn <- dots[[i]]

    fn_sig <- list(
      params = list(pkg_data_class(name), pkg, resource_class),
      return = class
    )

    names(fn_sig$params) <-
      utils::head(names(formals(fn)), length(fn_sig$params))

    method(pkg_data_get_scopes, pkg_data_class(name)) <-
      function(field) scopes

    method(pkg_data_get_description, pkg_data_class(name)) <-
      function(field) description

    method(pkg_data_get_tags, pkg_data_class(name)) <-
      function(field) tags

    method(pkg_data_get_suggests, pkg_data_class(name)) <-
      function(field) suggests

    method(pkg_data_get_derive_signature, pkg_data_class(name)) <-
      function(field) fn_sig

    method(pkg_data_derive, fn_sig$params) <-
      function(field, pkg, resource, scopes = opt("scopes"), ...) {
        assert_scopes(field, scopes)
        assert_suggests(field)

        out <- tryCatch(
          convert(fn(field, pkg, resource, ...), return_class),
          error = pkg_data_derive_error
        )

        assert_is(out, return_class_or_derive_error)
        out
      }
  }
}

register_metric <- function(
  name,
  class,
  ...,
  tags = pkg_data_tags()
) {
  assert_class_is(class, pkg_metric)
  register_data(
    name = name,
    class = class,
    tags = tags,
    ...
  )
}
