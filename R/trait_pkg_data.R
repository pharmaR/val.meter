#' Package Data Trait
#'
#' A trait is a collection of methods. Implemented a new piece of package data
#' requires that all of the following methods are implemented or have a provided
#' default implementation.
#'
#' Although it's possibly to implement all of these methods one by one, it will
#' be easier to use [`impl_data()`], which takes only the expected data and
#' uses it to implemented each method.
#'
#' @name pkg_data
NULL



get_data_derive_field_names <- function() {
  signature_prefix <- "pkg_data_field_"

  # get dispatch class types for first argument (field name)
  # must match signature of pkg_data_derive
  dispatch_args <- list("pkg" = class_pkg, "resource" = class_resource)
  methods <- pkg_data_derive@methods

  # will only subset methods for S7 dispatch
  for (arg in dispatch_args) methods <- methods[[class_desc(arg)]]

  # extract fields for which derivation is implemented given dispatch args
  signature_types <- names(methods)

  # subset for only methods which dispatch on [`pkg_data()`] types
  is_pkg_data <- startsWith(signature_types, signature_prefix)
  signature_types <- signature_types[is_pkg_data]

  # remove prefix for completions, return only those that match pattern
  substring(signature_types, nchar(signature_prefix) + 1L)
}



#' @describeIn pkg_data
#' Derive a [`pkg`] data field, giving a function by which a piece of package
#' data is calculated. This function is not called directly, it used by the
#' indexing functions implemented for [`pkg`] to populate new data fields.
#' Default implementations accept `field` as a `character`, automatically
#' dispatching on the field as a class object, and accept a missing
#' `resource`, instead iterating through `pkg` resources by priority.
#'
#' @export
pkg_data_derive <- new_generic("pkg_data_derive", c("pkg", "resource", "field"))

#' Derive by Field Name
#'
#' When a field is provided by name, create an empty S3 object using the field
#' name for further dispatch.
#'
#' @noRd
method(
  pkg_data_derive,
  list(class_any, new_union(class_any, class_missing), class_character)
) <-
  function(pkg, resource, field, ...) {
    pkg_data_derive(
      pkg,
      resource = NULL,
      field = as_pkg_data(field),
      ...,
      field_name = field
    )
  }

#' Derive using Pkg Resources
#'
#' If no explicit resource is provided, iterate through available package
#' resources and return the value using the first successful resource.
#'
#' @noRd
method(
  pkg_data_derive,
  list(class_pkg, new_union(NULL, class_missing), class_any)
) <-
  function(pkg, resource, field, ...) {
    pkg_data_derive(pkg = pkg, resource = pkg@resource, field = field, ...)
  }

method(
  pkg_data_derive,
  list(class_pkg, class_multi_resource, class_any)
) <-
  function(pkg, resource, field, ..., field_name) {
    for (resource in resource@resources) {
      result <- tryCatch(
        pkg_data_derive(
          pkg = pkg,
          resource = resource,
          field = field,
          ...,
          field_name = field_name
        ),
        error = identity
      )

      if (!inherits(result, "S7_error_method_not_found")) {
        return(result)
      }
    }

    new_err(
      class = "missing_derivation",
      "Package data '{field_name}' could not be derived from known resources"
    )
  }

#' Derive for Mock Resource
#'
#' Individual data implementations might provide their own functions for mock
#' resources, but for those that don't we can provide sensible defaults based
#' on the expected return type.
#'
#' @noRd
method(
  pkg_data_derive,
  list(class_pkg, class_mock_resource, pkg_data_class())
) <-
  function(pkg, resource, field, ...) {
    info <- pkg_data_info(field)
    pkg_data_derive(pkg, resource, info@type, ...)
  }

method(
  pkg_data_derive,
  list(class_any, class_mock_resource, new_union(.s7_class, .s7_base_class))
) <-
  function(pkg, resource, field, ...) {
    pkg_data_derive(
      pkg = pkg,
      resource = resource,
      field = structure(list(), class = c(field$class, class(field))),
      ...,
      field_name = field
    )
  }

method(
  pkg_data_derive,
  list(class_any, class_mock_resource, new_S3_class("logical"))
) <-
  function(pkg, resource, field, ...) sample(c(TRUE, FALSE), 1)

method(
  pkg_data_derive,
  list(class_any, class_mock_resource, new_S3_class("integer"))
) <-
  function(pkg, resource, field, ...) rpois(n = 1, lambda = 2)

method(
  pkg_data_derive,
  list(class_any, class_mock_resource, new_S3_class("numeric"))
) <-
  function(pkg, resource, field, ...) runif() ^ rpois(n = 1, lambda = 2)

#' @describeIn pkg_data
#' Retrieve metadata about a data field.
#'
#' @export
pkg_data_info <- new_generic("pkg_data_info", c("field"))

method(pkg_data_info, class_character) <-
  function(field) pkg_data_info(as_pkg_data(field))



#' @describeIn pkg_data
#' Helper for implementing all the necessary methods for package data.
#' Internally this is a wrapper for `[impl_data_meta()]` (associate metadata
#' with the data field), `[impl_data_derive()]` (associate a derivation
#' function for a combination of data field _and_ package resource) and
#' `[impl_metric()]` (declare a piece of data to be a metric).
#'
#' @export
impl_data <- function(name, fn, for_resource = resource, ..., metric = FALSE) {
  if (...length() > 0L)
    impl_data_info(name = name, ..., metric = metric)

  if (!missing(fn))
    impl_data_derive(name = name, fn = fn, resource = for_resource)
}


#' @describeIn pkg_data
#' Associate metadata with the data field
#'
#' @include class_tags.R
#' @include class_permissions.R
#' @include utils_s7_convert.R
#' @include trait_pkg_data_s3.R
#' @export
impl_data_info <- function(
  name,
  class = class_any,
  description = "",
  tags = class_tags(c()),
  permissions = class_permissions(c()),
  suggests = character(0L),
  metric = FALSE
) {
  tags <- convert(tags, class_tags)
  description <- convert(description, class_character)
  permissions <- convert(permissions, class_permissions)

  if (is.character(class))
    class <- S7::new_S3_class(class)

  class <- S7::as_class(class)

  method(pkg_data_info, pkg_data_class(name)) <-
    function(field) {
      data_info(
        metric = metric,
        description = description,
        tags = tags,
        type = class,
        suggests = suggests,
        scopes = permissions
      )
    }
}

#' @describeIn pkg_data
#' Register a derivation function for a data field and package resource.
#'
#' @include utils.R
#' @include utils_assertions.R
#' @include trait_pkg_data_s3.R
#' @export
impl_data_derive <- function(name, fn, resource) {
  method(pkg_data_derive, list(pkg, resource, pkg_data_class(name))) <-
    function(pkg, resource, field, ...) {
      info <- pkg_data_info(field)
      required_scopes <- info@scopes
      required_suggests <- info@suggests
      assert_scopes(required_scopes, pkg@scopes)
      assert_suggests(required_suggests)
      convert(fn(pkg, resource, field, ...), info@type)
    }
}
