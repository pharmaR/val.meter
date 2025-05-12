#' Derive Package Data
#'
#' Package datacan be any relevant bit of data pertaining to a package.
#' This can be intrinsic qualities of the package like data read directly from
#' a description file or the number of vignette files bundled with the package
#' itself. It can also be derived from executing the package code, such as
#' unit test results, or pulled from external sources such as the number of
#' lifetime package downloads.
#'
#' @examples
#' p <- pkg_from("glue")
#' derive("desc", p)
#'
#' @export
pkg_data_derive <- new_generic("pkg_data_derive", c("field", "pkg", "resource"))

#' @export
pkg_data_get_derive_signature <-
  new_generic("pkg_data_get_derive_signature", c("field"))

method(pkg_data_get_derive_signature, class_character) <-
  function(field) pkg_data_get_derive_signature(as_pkg_data(field))

#' @export
pkg_data_get_description <- new_generic("pkg_data_get_description", c("field"))

method(pkg_data_get_description, class_character) <-
  function(field) pkg_data_get_description(as_pkg_data(field))

#' @export
pkg_data_get_tags <- new_generic("pkg_data_get_tags", c("field"))

method(pkg_data_get_tags, class_character) <-
  function(field) pkg_data_get_tags(as_pkg_data(field))

#' @export
pkg_data_get_scopes <- new_generic("pkg_data_get_scopes", c("field"))

method(pkg_data_get_scopes, class_character) <-
  function(field, scopes, ...) pkg_data_get_scopes(as_pkg_data(field))

method(pkg_data_get_scopes, S7::new_S3_class(pkg_data_s3_class())) <-
  function(field, scopes, ...) pkg_data_scopes()  # default, no scopes


#' @export
pkg_data_get_suggests <- new_generic("pkg_data_get_suggests", c("field"))

method(pkg_data_get_suggests, class_any) <-
  function(field, ...) character(0L)  # default, no suggests

method(pkg_data_get_suggests, class_character) <-
  function(field, ...) pkg_data_get_suggests(as_pkg_data(field))


#' @export
pkg_data_is_metric <- new_generic("pkg_data_is_metric", c("field"))

method(pkg_data_is_metric, class_any) <- function(field) FALSE

method(pkg_data_is_metric, class_character) <-
  function(field) pkg_data_is_metric(as_pkg_data(field))


#' Create a Package Object
#'
#' @examples
#' p <- pkg_from("glue")
#'
#' @export
pkg_from <- new_generic("pkg_from", "x")


is <- new_generic("is", c("x", "class"))

method(is, list(class_any, class_any)) <-
  function(x, class) is(x, S7::as_class(class))

method(is, list(class_any, new_S3_class("S7_base_class"))) <-
  function(x, class) inherits(x, class$class)

method(is, list(class_any, new_S3_class("S7_S3_class"))) <-
  function(x, class) inherits(x, class$class)

method(is, list(new_S3_class("S7_object"), new_S3_class("S7_class"))) <-
  function(x, class) {
    S7::S7_inherits(x = x, class = class)
  }

method(is, list(class_any, new_S3_class("S7_union"))) <-
  function(x, class) {
    any(vlapply(class$classes, x = x, is))
  }


is_subclass <- new_generic("is_subclass", c("subclass", "class"))

method(is_subclass, list(class_any, class_any)) <-
  function(subclass, class) FALSE

method(is_subclass, list(new_S3_class("S7_base_class"), new_S3_class("S7_base_class"))) <-
  function(subclass, class) {
    identical(subclass, class)
  }

method(is_subclass, list(new_S3_class("S7_class"), new_S3_class("S7_class"))) <-
  function(subclass, class) {
    if (identical(subclass, class)) return(TRUE)
    is_subclass(subclass@parent, class)
  }

method(is_subclass, list(class_any, new_S3_class("S7_union"))) <-
  function(subclass, class) {
    any(vlapply(class$classes, subclass = subclass, is_subclass))
  }
