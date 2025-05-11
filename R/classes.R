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
pkg_data_scopes <- new_class(
  "pkg_data_scopes",
  parent = class_character,
  validator = local({
    all_scopes <- c(
      "permit_version_independent",
      "permit_transient",
      "permit_computed",
      "permit_network"
    )

    function(self) {
      if (!all(self %in% all_scopes)) {
        "Data tags must be one of provided scopes"
      }
    }
  })
)



#' Package Data Tags Class
#'
#' A enumeration of tags that can be applied to a metric, to capture metadata
#' more effectively.
#'
#' @export
pkg_data_tags <- new_class(
  "pkg_data_tags",
  parent = class_character,
  validator = local({
    accepted_tags <- c(
      "adoption",
      "lifecycle management",
      "best practice",
      "execution"
    )

    function(self) {
      if (!all(self %in% accepted_tags)) {
        "Data tags must be one of accepted tags"
      }
    }
  })
)



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
pkg <- new_class(
  "pkg",
  properties = list(
    #' @field data A mutable environment, used to aggregate package meatadata
    data = class_environment,

    #' @field resources A list of resources of package data for the
    #' particular package. The order of the list determines the precedence
    #' of information. If information about a package could be derived from
    #' multiple sources, the first source is prioritized.
    resources = class_list,

    #' @field scopes Permissible scopes for deriving data.
    scopes = pkg_data_scopes
  )
)

#' @exportS3Method ".DollarNames" "val.meter::pkg"
`.DollarNames.val.meter::pkg` <- function(x, pattern, ...) { # nolint: object_name_linter, line_length.
  signature_prefix <- "pkg_data_field_"

  # get dispatch class types for first argument (field name)
  signature_types <- names(pkg_data_derive@methods)

  # subset for only methods which dispatch on [`pkg_data()`] types
  is_pkg_data <- startsWith(signature_types, signature_prefix)
  signature_types <- signature_types[is_pkg_data]

  # remove prefix for completions, return only those that match pattern
  fields <- substring(signature_types, nchar(signature_prefix) + 1L)
  fields[grepl(pattern, fields)]
}

get_pkg_data <- function(x, name, ...) {
  .trace$set_bottom(1L)
  if (!exists(name, envir = x@data))
    x@data[[name]] <- pkg_data_derive(name, x)
  x@data[[name]]
}

#' @exportS3Method "$" "val.meter::pkg"
`$.val.meter::pkg` <- get_pkg_data

#' @exportS3Method "$" "val.meter::pkg"
`[[.val.meter::pkg` <- get_pkg_data
