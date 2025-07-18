#' Package Data
#' 
#' Package data is feature of `r packageName()` - it is the framework by which
#' we calculate package qualities and derive metrics. Package data is any
#' derived information about a package. Fundamentally, new data is implemented
#' using the methods [`pkg_data_derive()`] and [`pkg_data_info()`] used for
#' calculating and providing metadata respectively.
#' 
#' However, implementing these functions directly requires a bit of knowledge
#' of how the internals of the package are structured. Instead, it is
#' recommended to use [`impl_data()`], which provides a high-level interface
#' that helps make this process as simple as possible.
#' 
#' @section Implementing a new metric:
#' 
#' To implement some new data, you can use [`impl_data()`], providing, at a
#' minimum, the field name for the data and the way it should be calculated.
#' 
#'     impl_data("name_character_count", function(pkg, ...) nchar(pkg$name))
#'
#' @examples
#' p <- random_pkg()
#' impl_data("name_character_count", function(pkg, ...) nchar(pkg$name))
#' p$name_character_count
#'
#'
#' @include generic_pkg_data_derive.R
#' @include generic_pkg_data_info.R
#' @include utils_pkg_data_dispatch.R
#' @name pkg_data
NULL

#' @describeIn pkg_data
#' Helper for implementing all the necessary methods for package data.
#' Internally this is a wrapper for `[impl_data_meta()]` (associate metadata
#' with the data field), `[impl_data_derive()]` (associate a derivation
#' function for a combination of data field _and_ package resource) and
#' `[impl_metric()]` (declare a piece of data to be a metric).
#'
#' @export
impl_data <- function(
  name, 
  fn,
  for_resource = resource,
  ...,
  metric = FALSE,
  overwrite = FALSE
) {
  is_info_impl <- is_implemented(pkg_data_info, pkg_data_class(name))
  if (is_info_impl && ...length() && !overwrite) {
    stop(
      "data info for '", name, "' is already implemented. Use ",
      "overwrite=TRUE to modify."
    )
  }
  
  if (!is_info_impl || overwrite) {
    impl_data_info(name = name, ..., metric = metric, overwrite = overwrite)
  }
  
  if (!missing(fn)) {
    impl_data_derive(name = name, fn = fn, resource = for_resource)
  }
}


#' @describeIn pkg_data
#' Associate metadata with the data field
#'
#' @include class_tags.R
#' @include class_permissions.R
#' @export
impl_data_info <- function(
  name,
  class = class_any,
  title = character(0L),
  description = character(0L),
  tags = class_tags(c()),
  permissions = class_permissions(character(0L)),
  suggests = class_suggests(character(0L)),
  metric = FALSE,
  overwrite = FALSE
) {
  tags <- convert(tags, class_tags)
  suggests <- convert(suggests, class_suggests)
  description <- convert(description, class_character)
  permissions <- convert(permissions, class_permissions)

  if (is.character(class)) {
    class <- S7::new_S3_class(class)
  }

  class <- S7::as_class(class)

  method(pkg_data_info, pkg_data_class(name)) <-
    function(field) {
      data_info(
        metric = metric,
        title = title,
        description = description,
        tags = tags,
        data_class = class,
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
#' @export
impl_data_derive <- function(name, fn, resource) {
  method(pkg_data_derive, list(pkg, resource, pkg_data_class(name))) <-
    function(pkg, resource, field, ...) {
      info <- pkg_data_info(field)
      required_scopes <- info@scopes
      required_suggests <- info@suggests
      assert_scopes(required_scopes, pkg@scopes)
      assert_suggests(required_suggests)
      
      data <- fn(pkg, resource, field, ...)
      if (!identical(info@data_class, class_any)) {
        data <- convert(data, info@data_class)
      }
      
      data
    }
}
