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
    new_object(resources = convert(x, resource), scopes = scopes)
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

    x@data[[name]] <- pkg_data_derive(name, x)
  }

  if (.raise && inherits(x@data[[name]], cnd_type())) err(
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
      err("pkg objects can only be indexed with scalar logical values")
    return(x[names(metrics(all = all))])
  }

  err("pkg objects don't know how to index with class {.cls index}")
}
