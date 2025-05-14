#' Package Data Scopes Class
#'
#' A descriptor of behaviors required for deriving package data. Each policy
#' property is a [`logical`] flag, which may be either `TRUE` or `FALSE`.
#' Policies are declared for each metric, as well as when deriving metrics for
#' a package. When deriving metrics, only those metrics which conform to the set
#' policy will be derived.
#'
#' Given a policy, only assessments which conform to all enabled flags will be
#' assessed.
#'
#' @export
pkg_data_permissions <- local({
  values <- c(
    "execution",
    "network"
  )

  new_class(
    "pkg_data_permissions",
    parent = class_character,
    constructor = function(...) {
      n <- ...length()
      value <- if (n == 0L) {
        values
      } else if (n == 1L && identical(..1, TRUE)) {
        values
      } else if (n == 1L && identical(..1, FALSE)) {
        character(0L)
      } else {
        as.character(c(...))
      }

      new_object(value)
    },
    validator = function(self) {
      if (!all(self %in% values)) {
        fmt(
          "Data permissions must be one of provided scopes: \n",
          "  {.str {permissions}}"
        )
      }
    }
  )
})

#' Package Data Tags Class
#'
#' A enumeration of tags that can be applied to a metric, to capture metadata
#' more effectively.
#'
#' @export
pkg_data_tags <- local({
  values <- c(
    "adoption",
    "lifecycle management",
    "best practice",
    "execution",
    "transient",
    "version-independent"
  )

  new_class(
    "pkg_data_tags",
    parent = class_character,
    constructor = function(...) {
      n <- ...length()
      value <- if (n == 0L) {
        values
      } else if (n == 1L && identical(..1, TRUE)) {
        values
      } else if (n == 1L && identical(..1, FALSE)) {
        character(0L)
      } else {
        as.character(c(...))
      }

      new_object(value)
    },
    validator = function(self) {
      if (!all(self %in% values)) {
        fmt(
          "Data tags must be one of provided tag names: \n",
          "  {.str {permissions}}"
        )
      }
    }
  )
})


#' Package Data Function Class
#'
#' A wrapper around a function, which allows binding required scopes.
#'
#' @export
pkg_data_derive_error <- new_class(
  "pkg_data_derive_error",
  parent = S7::new_S3_class("error")
)



#' Package Class
#'
#' Aggregates package information, surveying metadata from discovered
#' resources.
#'
#' @export
new_pkg <- new_class(
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

    #' @field resources A list of resources of package data for the
    #' particular package. The order of the list determines the precedence
    #' of information. If information about a package could be derived from
    #' multiple sources, the first source is prioritized.
    resources = class_list,

    #' @field scopes Permissible scopes for deriving data.
    scopes = pkg_data_permissions
  )
)

#' @exportS3Method ".DollarNames" "val.meter::pkg"
`.DollarNames.val.meter::pkg` <- function(x, pattern, ...) {
  fields <- get_data_derive_field_names()
  fields[grepl(pattern, fields)]
}

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

  if (.raise && inherits(x@data[[name]], err_type())) err(
    class = "derive_dependency",
    data = list(field = name),
    "field depends on field '{name}' that threw an error during derivation"
  )

  x@data[[name]]
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
