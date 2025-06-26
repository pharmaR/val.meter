#' Package Class
#'
#' Aggregates package information, surveying metadata from discovered
#' resources.
#'
#' @include class_resource.R
#' @export
new_pkg <- class_pkg <- new_class(
  "pkg",
  properties = list(
    #' @field data A mutable environment, used to aggregate package meatadata.
    #' When working with a [`pkg`] object, you may prefer to use the `[[` and
    #' `$` operators for accessing data in this environment, which will also
    #' prompt any necessary data dependencies to be evaluated.
    data = class_environment,

    #' @field metrics Return calculated listing of all metrics
    metrics = new_property(
      class_any,
      getter = function(self) self[TRUE]
    ),

    #' @field resource A [`resource`] (often a [`multi_resource`]), providing
    #' the resources to be used for deriving packages data. If a
    #' [`multi_resource`], the order of resources determines the precedence
    #' of information. If information about a package could be derived from
    #' multiple sources, the first source is prioritized.
    resource = class_resource,

    #' @field scopes Permissible scopes for deriving data.
    scopes = permissions
  ),
  constructor = function(x, scopes = opt("permissions")) {
    new_object(
      .parent = S7::S7_object(),
      data = new.env(parent = emptyenv()),
      metrics = list(),
      resource = convert(x, resource),
      scopes = scopes
    )
  }
)

get_pkg_data <- function(x, name, ..., .raise = .state$raise) {
  if (!exists(name, envir = x@data)) {
    # upon computing subsequent data dependencies, raise their errors so that
    # they can be captured and annotated as dependency errors
    if (!.raise) {
      .state$raise_derive_errors()
      on.exit(.state$raise_derive_errors(FALSE))
    }

    x@data[[name]] <- tryCatch(pkg_data_derive(name, x), error = identity)

    if (.raise && inherits(x@data[[name]], "error")) {
      stop(x@data[[name]])
    }
  }

  if (.raise && inherits(x@data[[name]], cnd_type())) new_err(
    class = "derive_dependency",
    data = list(field = name),
    "field depends on field '{name}' that threw an error during derivation"
  )

  x@data[[name]]
}

#' @exportS3Method ".DollarNames" "val.meter::pkg"
`.DollarNames.val.meter::pkg` <- function(x, pattern, ...) {
  fields <- get_data_derive_field_names()
  fields[grepl(pattern, fields)]
}

#' @exportS3Method "$" "val.meter::pkg"
`$.val.meter::pkg` <- get_pkg_data

#' @exportS3Method "[[" "val.meter::pkg"
`[[.val.meter::pkg` <- get_pkg_data

#' @exportS3Method "[" "val.meter::pkg"
`[.val.meter::pkg` <- function(x, index, ..., all = FALSE) {
  if (is.character(index)) {
    names(index) <- index
    return(lapply(index, function(i) x[[i]]))
  }

  if (is.logical(index)) {
    if (length(index) != 1)
      new_err("pkg objects can only be indexed with scalar logical values")
    return(x[names(metrics(all = all))])
  }

  new_err("pkg objects don't know how to index with class {.cls index}")
}

method(print, class_pkg) <- function(x, ...) {
  class_header <- paste0("<", class(x)[[1]], ">")

  fields <- get_data_derive_field_names()
  names(fields) <- fields
  fields <- lapply(fields, convert, to = metric)
  is_metric <- vlapply(fields, S7::prop, "metric")
  fields <- fields[order(!is_metric)]

  out <- paste0(
    class_header, "\n",
    "@resources", "\n",
    paste0("  ", capture.output(x@resource), collapse = "\n"), "\n",
    "@scopes", "\n",
    paste0("  ", capture.output(x@scopes), collapse = "\n"), "\n",
    paste0(
      collapse = "\n",
      "$", names(fields), "\n",
      vcapply(names(fields), function(field) {
        paste("  ", collapse = "\n", if (exists(field, x@data)) {
          capture.output(x@data[[field]])
        } else {
          "not yet derived ..."
        })
      })
    )
  )

  cat(out, "\n")
}

#' @include utils_dcf.R
#' @export
method(encode_dcf, class_pkg) <- function(x, ...) {
  c(
    encode_dcf(x@resource),
    paste0(names(x@metrics), ": ", vcapply(x@metrics, encode_dcf))
  )
}

#' @include utils_dcf.R
#' @export
method(decode_dcf, class_pkg) <- function(x, ...) {
  
}
