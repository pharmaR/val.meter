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
  signature_types <- names(pkg_data_derive@methods)

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
pkg_data_derive <- new_generic("pkg_data_derive", c("field", "pkg", "resource"))

#' Derive by Field Name
#'
#' When a field is provided by name, create an empty S3 object using the field
#' name for further dispatch.
#'
#' @noRd
method(
  pkg_data_derive,
  list(class_character, class_any, new_union(class_any, class_missing))
) <-
  function(field, pkg, resource, ...) {
    field <- as_pkg_data(field)
    pkg_data_derive(field, pkg, resource = NULL, ..., field_name = field)
  }

#' Derive using Pkg Resources
#'
#' If no explicit resource is provided, iterate through available package
#' resources and return the value using the first successful resource.
#'
#' @noRd
method(
  pkg_data_derive,
  list(class_any, class_pkg, new_union(NULL, class_missing))
) <-
  function(field, pkg, resource, ..., field_name) {
    pkg_data_derive(
      field = field,
      pkg = pkg,
      resource = pkg@resource,
      ...,
      field_name = field_name
    )
  }

method(
  pkg_data_derive,
  list(class_any, class_pkg, class_multi_resource)
) <-
  function(field, pkg, resource, ..., field_name) {
    for (resource in resource@resources) {
      result <- tryCatch(
        pkg_data_derive(
          field = field,
          pkg = pkg,
          resource = resource,
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



#' @describeIn pkg_data
#' Retrieve the class of the data field. When implemented using [`impl_data()`],
#' the result of calling [`pkg_data_derive()`] is automatically converted into
#' this class, raising errors if conversion fails. The default implementation
#' returns [`S7::class_any`], though it is recommended that a more precise
#' class should be specified.
#'
#' @export
pkg_data_get_class <- new_generic("pkg_data_get_class", c("field"))

method(pkg_data_get_class, class_character) <-
  function(field) pkg_data_get_class(as_pkg_data(field))

method(pkg_data_get_class, S7::new_S3_class(pkg_data_s3_class())) <-
  function(field) class_any



#' @describeIn pkg_data
#' Retrieve a description of the data field. Descriptions should be roughly one
#' sentence and for metrics, provide necessary information for interpretting the
#' result. The default implmentation provides no description.
#'
#' @export
pkg_data_get_description <- new_generic("pkg_data_get_description", c("field"))

method(pkg_data_get_description, class_character) <-
  function(field) pkg_data_get_description(as_pkg_data(field))

method(pkg_data_get_description, S7::new_S3_class(pkg_data_s3_class())) <-
  function(field) ""


#' @describeIn pkg_data
#' Retrieve a [`tags()`] object describing tags associated with the data field.
#' The default implementation lists no tags.
#'
#' @export
pkg_data_get_tags <- new_generic("pkg_data_get_tags", c("field"))

method(pkg_data_get_tags, class_character) <-
  function(field) pkg_data_get_tags(as_pkg_data(field))

method(pkg_data_get_tags, S7::new_S3_class(pkg_data_s3_class())) <-
  function(field, scopes, ...) tags(FALSE)  # default, no tags



#' @describeIn pkg_data
#' Retrieve a [`permissions()`] object describing necessary permissions required
#' to compute the data. The default implementation lists no required
#' permissions.
#'
#' @export
pkg_data_get_permissions <- new_generic("pkg_data_get_scopes", c("field"))

method(pkg_data_get_permissions, class_character) <-
  function(field, scopes, ...) pkg_data_get_permissions(as_pkg_data(field))

method(pkg_data_get_permissions, S7::new_S3_class(pkg_data_s3_class())) <-
  function(field, scopes, ...) permissions(FALSE)  # default, no scopes



#' @describeIn pkg_data
#' Retrieve a vector of any `Suggests` dependencies required for deriving the
#' data. The default implementation returns no dependencies.
#'
#' @export
pkg_data_get_suggests <- new_generic("pkg_data_get_suggests", c("field"))

method(pkg_data_get_suggests, class_any) <-
  function(field, ...) character(0L)  # default, no suggests

method(pkg_data_get_suggests, class_character) <-
  function(field, ...) pkg_data_get_suggests(as_pkg_data(field))



#' @describeIn pkg_data
#' Retrieve a `logical` flag, indicating whether the data field is a `metric`.
#' When implemented using [`impl_data()`], defining a field of data as a metric
#' also enforces that its return type is atomic. The default implementation
#' always returns `FALSE`.
#'
#' @export
pkg_data_is_metric <- new_generic("pkg_data_is_metric", c("field"))

method(pkg_data_is_metric, class_any) <- function(field) FALSE

method(pkg_data_is_metric, class_character) <-
  function(field) pkg_data_is_metric(as_pkg_data(field))



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
    impl_data_meta(name = name, ...)

  if (!missing(fn))
    impl_data_derive(name = name, fn = fn, resource = for_resource)

  if (metric)
    impl_metric(name)
}


#' @describeIn pkg_data
#' Associate metadata with the data field
#'
#' @include class_tags.R
#' @include class_permissions.R
#' @include utils_s7_convert.R
#' @include trait_pkg_data_s3.R
#' @export
impl_data_meta <- function(
  name,
  class = class_any,
  description = "",
  tags = class_tags(c()),
  permissions = class_permissions(c()),
  suggests = character(0L)
) {
  tags <- convert(tags, class_tags)
  description <- convert(description, class_character)
  permissions <- convert(permissions, class_permissions)

  if (is.character(class))
    class <- S7::new_S3_class(class)

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

#' @describeIn pkg_data
#' Register a derivation function for a data field and package resource.
#'
#' @include utils.R
#' @include utils_assertions.R
#' @include trait_pkg_data_s3.R
#' @export
impl_data_derive <- function(name, fn, resource) {
  return_class <- pkg_data_get_class(name)
  method(pkg_data_derive, list(pkg_data_class(name), new_pkg, resource)) <-
    function(field, pkg, resource, ...) {
      assert_scopes(field, pkg@scopes)
      assert_suggests(field)
      convert(fn(field, pkg, resource, ...), return_class)
    }
}

#' @describeIn pkg_data
#' Declare a data field to be a metric, enforcing that the derivation return
#' type is atomic.
#'
#' @include utils.R
#' @include utils_assertions.R
#' @include trait_pkg_data_s3.R
#' @export
impl_metric <- function(name, class = pkg_data_get_class(name)) {
  assert_metric_is_atomic(class)
  method(pkg_data_is_metric, pkg_data_class(name)) <-
    function(field) TRUE
}
