#' Package data derivation error handling
#'
#' Because we aim to capture errors that are raised when evaluating packages,
#' there is a fair bit of machinery to raise relevant errors when package
#' assertions fail and capture errors during data execution.
#'
#' @keywords internal
#' @name errors
NULL

class_val_meter_error <- S7::new_S3_class("val_meter_error")
class_error <- S7::new_S3_class("error")

#' @describeIn errors
#' Global error raising flags
#'
#' This flag is used to determine when errors during execution should be
#' captured or thrown to the evaluating environment. When data is being
#' derived for the first time, we want to capture any errors, but when errors
#' arrive during other uses of that data, we want to raise them to the user.
#'
.state <- local({
  raise <- FALSE

  raise_derive_errors <- function(value = TRUE) {
    raise <<- value
    once_on_task_callback("raise", raise <<- FALSE)
  }

  environment()
})

#' @describeIn errors
#' Used to reset global state after a top level callback completes
once_on_task_callback <- function(name, expr, envir = parent.frame()) {
  addTaskCallback(
    name = paste0(packageName(), "-clear-trace-", name),
    f = function(...) {
      eval(expr, envir = envir)
      FALSE # don't persist after next callback
    }
  )
}

#' @describeIn errors
#' Walk the call stack to find the last call before the package boundary. This
#' allows us to raise the most relevant parts of error messages back to users
#' without exposing them to the internal non-standard evaluation calls.
get_package_boundary_call <- function(calls = sys.calls()) {
  for (i in seq_len(sys.nframe())) {
    if (identical(parent.env(sys.frame(i)), topenv())) {
      return(sys.frame(i))
    }
  }

  sys.frame()
}

#' @describeIn errors
#' Create a condition type
cnd_type <- function(class = NULL, cnd = "error") {
  prefix <- gsub(".", "_", packageName(), fixed = TRUE)
  c(
    sprintf("%s_%s_%s", prefix, class, cnd),
    paste(prefix, cnd, sep = "_")
  )
}

#' @describeIn errors
#' Extract a condition class from a type
cnd_class_from_type <- function(type, cnd = "error") {
  prefix <- gsub(".", "_", packageName(), fixed = TRUE)
  re <- paste0("^", prefix, "_?", "(.*)", "_", cnd, "$")
  gsub(re, "\\1", type)
}

#' @describeIn errors
#' Create a new error
#'
#' This function is a wrapper around [`cli::cli_abort()`], but with defaults
#' that make typical use within this package more interpretable to end-users.
#'
#' Used predominately by [`err()`]
new_err <- function(
  ...,
  data = list(),
  class = NULL,
  call = .envir,
  trace = NULL,
  capture = FALSE,
  .envir = parent.frame()
) {
  topenv_frame_idx <- Position(
    function(frame) identical(frame, topenv()),
    sys.frames(),
    nomatch = sys.nframe()
  )

  args <- list(data = data)
  args$message <- as.character(list(...))
  args$call <- get_package_boundary_call()
  args$class <- cnd_type(class)
  args$trace <- trace
  args$call <- call
  args$.envir <- .envir
  args$.trace_bottom <- sys.frames()[[topenv_frame_idx]]
  
  if (capture) {
    return(tryCatch(do.call(cli::cli_abort, args), error = identity))
  }
  
  do.call(cli::cli_abort, args)
}

#' @describeIn errors
#' Raise a new error, using one of a set of known error types
err <- list(
  #' @field disallowed_scopes Create an error indicating that a data derivation
  #'   requires scopes that were not permitted at execution time.
  disallowed_scopes = function(scopes, ...) {
    data <- list(scopes = scopes)
    stopifnot(is.character(data$scopes))
    new_err(
      class = "disallowed_scopes",
      data = data,
      "data derivation requires disallowed scopes: {.str {data$scopes}}",
      ...
    )
  },
  
  #' @field missing_suggests Create an error indicating that a dependency that
  #'   is required for a specific data derivation is not available.
  missing_suggests = function(suggests, ...) {
    data <- list(suggests = suggests)
    data$suggests <- suggests
    stopifnot(is.character(data$suggests))

    new_err(
      class = "missing_suggests",
      data = data,
      "data derivation requires suggests: {.pkg {data$suggests}}",
      ...
    )
  },
  
  #' @field metric_not_atomic Create an error indicating that a metric was
  #'   derived but did not conform to its anticipated atomic return type.
  metric_not_atomic = function(...) {
    new_err(
      class = "metric_not_atomic",
      data = list(),
      "metric computation did not produce an atomic value",
      ...
    )
  },
  
  #' @field derive_dependency Create an error that is raised when a dependent
  #'   data field threw an error during execution.
  derive_dependency = function(field, ...) {
    data <- list(field = field)
    stopifnot(is.character(data$field))
    new_err(
      class = "derive_dependency",
      data = data,
      paste0(
        "field depends on field {.str {data$field}} that threw an error during", 
        "derivation"
      ),
      ...
    )
  },
  
  #' @field data_not_implemented Create an error indicating that data could not
  #'   be derived because it is not implemented for this resource.
  not_implemented_for_resource = function(resource, field, ...) {
    data <- list(resource = resource, field = field)
    stopifnot(
      is.character(data$resource),
      is.character(data$field)
    )
    
    new_err(
      class = "not_implemented_for_resource",
      data = data,
      paste0(
        "data field {.str {data$field}} could not be derived because it is ",
        "not implemented for resource {.cls {data$resource}}"
      ),
      ...
    )
  }
)

#' Build an error from an error type and data attributes
#'
#' @note This function is only intended for use when parsing serialized output
#'   from text.
#'
#' This function uses an error class to build an error object. It intentionally
#' produces incomplete error objects, lacking the error backtrace
#' call.
#'
#' For signalling errors internal to the package, see [`err()`]. This function
#' is used to provide a readable syntax to exported `PACKAGES` files, which
#' are parsed using this function back into their respective error objects.
#'
#' @examples
#' # given a DCF input such as
#' 
#' ## Package: testpkg
#' ## Version: 1.2.3
#' ## Metric/word_count@R: error("missing_suggests", "wordcount")
#' 
#' # we want to parse (by evaluation) the output into our own error type
#' error("missing_suggests", "wordcount")
#'
#' @export
error <- function(type, ...) { # nolint: object_usage_linter
  cnd <- tryCatch(do.call(err[[type]], list(...)), error = identity)
  cnd$trace <- NULL
  cnd$call <- NULL
  cnd
}

method(
  convert, 
  list(new_S3_class("S7_error_method_not_found"), class_val_meter_error)
) <- 
  function(from, to) {
    # S7 error doesn't include data directly, must be parsed out
    msg <- strsplit(from$message, "\n")[[1]]
    msg_generic <- msg[[1]]
    msg_resource <- msg[[length(msg) - 1L]]  # second-to-last dispatch arg
    msg_field <- msg[[length(msg)]]  # last dispatch arg
    
    # only wrap in our own error type for missing derive implementations
    generic_str <- sub(".*generic `([^`(]*).*", "\\1", msg_generic, perl = TRUE)
    if (generic_str != "pkg_data_derive") return(from)
    
    # extract resource class and field name from error message
    resource_str <- sub(".*<([^/>]*).*", "\\1", msg_resource, perl = TRUE)
    field_str <- sub(".*<([^/>]*).*", "\\1", msg_field, perl = TRUE)
    field <- pkg_data_name_from_s3_class(field_str)
    
    err$not_implemented_for_resource(
      resource = resource_str,
      field = field,
      capture = TRUE
    )
  }

method(
  convert, 
  list(class_error, class_val_meter_error)
) <- 
  function(from, to) {
    from
  }

#' @include utils_dcf.R
method(to_dcf, S7::new_S3_class("val_meter_error")) <- function(x, ...) {
  subclass <- cnd_class_from_type(class(x)[[1]])
  text_data <- lapply(x$data, to_dcf)

  paste0(
    packageName(), "::", "error(",
    '"', subclass, '"',
    if (length(text_data) > 0) ", ",
    paste0(names(text_data), " = ", text_data, collapse = ", "),
    ")"
  )
}

#' @include utils_dcf.R
method(to_dcf, S7::new_S3_class("condition")) <- function(x, ...) {
  stop("Condition type cannot be encoded to dcf format")
}

as_pkg_data_derive_error <- function(x, ...) {
  after <- match(cnd_type(), class(x), nomatch = 1L) - 1L
  class(x) <- append(class(x), cnd_type("derive")[[1L]], after = after)
  new_data <- list(...)
  x[names(new_data)] <- new_data
  x
}