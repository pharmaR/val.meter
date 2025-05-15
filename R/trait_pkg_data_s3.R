#' Various helpers for using a field name for method dispatch
#'
#' Given a field as a name, for example `"desc"`, we want to dispatch to the
#' right derivation method for this piece of data. To do this, we need to
#' convert this field name into an object that has a class that we can use
#' for method dispatch. These functions are helpers for standardizing this
#' conversion so that we don't need to concern ourselves with consistent
#' class names throughout this package.
#'
#' In most cases, to dispatch to a method for a field, one would
#'
#'     fn(as_pkg_data("field_name"), ...)
#'
#' Which creates an `S3` object with a corresponding class. For generics in this
#' package that might be dispatched by field name, they often have default
#' methods already implemented so that you can simplify this to:
#'
#'     fn("field_name", ...)
#'
#' And it will implicitly dispatch to the appropriate method.
#'
#' @name pkg_data_dispatch
#' @keywords internal
NULL

#' @describeIn pkg_data_dispatch
#' Convert a field name into an S3 class name for dispatch
pkg_data_s3_class <- function(field_name = c()) {
  base <- "pkg_data_field"
  c(paste(base, field_name, sep = "_"), base)
}

#' @describeIn pkg_data_dispatch
#' Convert a field name into an S7 S3 class object for dispatch
pkg_data_class <- function(field_name = NULL) {
  S7::new_S3_class(pkg_data_s3_class(field_name))
}

#' @describeIn pkg_data_dispatch
#' Convert a field name an object with appropriate class for dispatch
as_pkg_data <- function(field_name) {
  structure(field_name, class = pkg_data_s3_class(field_name))
}
