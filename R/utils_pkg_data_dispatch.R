#' Various helpers for using a field name for method dispatch
#'
#' Given a field as a name, for example `"name"`, we want to dispatch to the
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
#' @keywords internal
#' @name pkg_data_dispatch
NULL

#' @describeIn pkg_data_dispatch
#' Convert a field name into an S3 class name for dispatch
pkg_data_s3_class <- function(field_name = NULL, mock = FALSE) {
  base <- "pkg_data_field"
  if (mock) base <- paste0(base, "_mock")
  c(sprintf("%s_%s", base, field_name), base)
}

#' @describeIn pkg_data_dispatch
#' Convert a field name into an S7 S3 class object for dispatch
pkg_data_class <- function(...) {
  S7::new_S3_class(pkg_data_s3_class(...))
}

#' @describeIn pkg_data_dispatch
#' Parse a data field name from its S3 class. The inverse of 
#'   [`pkg_data_s3_class`].
pkg_data_name_from_s3_class <- function(class_name) {
  signature_prefix <- "pkg_data_field_"
  is_pkg_data_class <- startsWith(class_name, signature_prefix)
  ifelse(
    is_pkg_data_class,
    sub(paste0(signature_prefix, "(mock_)?"), "", class_name),
    NA_character_
  )
}

#' @describeIn pkg_data_dispatch
#' Convert a field name an object with appropriate class for dispatch
as_pkg_data <- function(field_name) {
  structure(field_name, class = pkg_data_s3_class(field_name))
}

#' Discover implemented data fields
#' 
#' This is used for finding all available metrics for use in [`metrics()`], as
#' well as for tab completions for `<pkg>$ <TAB>` to auto-populate a list of
#' available metrics.
#' 
#' @param ... A list of [`S7`] classes. Not used if `args` is provided.
#' @param args A list of [`S7`] classes, by default, collects the elements of
#'   `...`.
#' 
#' @returns A `character` vector of field names.
#' 
#' @keywords internal
#' @name pkg_data_dispatch
get_data_derive_field_names <- function(..., args = list(...)) {
  if (length(args) == 0L) args <- list(class_pkg, class_resource)
  
  all_methods <- function(x) {
    if (is.function(x) && inherits(x, "S7_generic")) {
      all_methods(x@methods)
    } else if (is.environment(x)) {
      unlist(recursive = FALSE, use.names = FALSE, lapply(x, all_methods))
    } else {
      x
    }
  }
  
  # retrieve all methods
  methods <- all_methods(pkg_data_derive)
  signatures <- lapply(methods, attr, "signature")
  valid_signatures <- Filter(x = signatures, function(signature) {
    for (i in seq_along(args))
      if (!is_subclass(signature[[i]], args[[i]]))
        return(FALSE)
    
    TRUE
  })
  
  # extract fields for which derivation is implemented given dispatch args
  signature_types <- unique(as.character(Filter(
    Negate(is.null), 
    lapply(signatures, function(signature) {
      signature[[3]]$class[[1]]
    })
  )))
  
  # extract parsed data field names from class names
  Filter(Negate(is.na), pkg_data_name_from_s3_class(signature_types))
}
