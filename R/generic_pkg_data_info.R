#' Derive package data information
#' 
#' 
#' Retrieve metadata about a data field.
#'
#' @family generics
#' @export
pkg_data_info <- new_generic("pkg_data_info", c("field"))

method(pkg_data_info, class_character) <-
  function(field) {
    pkg_data_info(as_pkg_data(field))
  }