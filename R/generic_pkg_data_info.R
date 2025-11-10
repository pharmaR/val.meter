#' Derive package data information
#'
#' Retrieve metadata about a data field.
#'
#' @param field `character(1L)` field name to retrieve.
#' @param resource [`resource`] providing the resource to be used for
#'   retrieving package data info.
#' @param ... Additional arguments unused.
#'
#' @return `data_info` for field `field`.
#'
#' @family generics
#' @export
pkg_data_info <- new_generic("pkg_data_info", c("field", "resource"))

method(pkg_data_info, list(class_character, class_missing)) <-
  function(field, resource, ...) {
    pkg_data_info(field, resource = class_resource)
  }

#' Aggregates field constraints across field dependencies.
#'
#' For example, `R CMD check` errors depend on `R CMD check`. Even though
#' the dependency `rcmdcheck` is only declared for the interal object, we want
#' to surface this information to users who want to derive the error count.
#'
#' This accessor surfaces the aggregate required suggests and permissions,
#' following the data dependencies.
#'
#' @note This method of deriving fields aggregates all dependencies based on
#' static code analysis of their implementation functions. If, for example,
#' fields are only used conditionally, this context is lost and the
#' field permissions and required suggested packages will be reported
#' from all code branches.
#'
#' @keywords internal
#' @noRd
method(pkg_data_info, list(class_character, class_any)) <-
  function(field, resource, ...) {
    fields <- field # fields to be processed
    field_dep_info <- list() # to be populated when crawling derive deps

    # walk fields discovered in derive functions to aggregate metadata
    while (length(fields) > 0L) {
      field <- fields[[1L]]
      field_dep_info[[field]] <- pkg_data_info(as_pkg_data(field))

      dispatch_args <- list(class_pkg, resource, pkg_data_class(field))
      fn <- tryCatch(
        method(pkg_data_derive, class = dispatch_args),
        error = function(...) NULL
      )

      fields <- append(fields, discover_data_derive_deps(fn))
      fields <- setdiff(fields, names(field_dep_info))
    }

    info <- field_dep_info[[1L]]
    permissions <- lapply(field_dep_info, S7::prop, "permissions")
    suggests <- lapply(field_dep_info, S7::prop, "suggests")
    info@permissions <- permissions(unique(unlist(permissions)))
    info@suggests <- suggests(unique(unlist(suggests)))

    info
  }

#' Search an expression for accessors of `pkg`
#'
#' Used to find data dependencies in a derive function. Uses of `pkg$name` and
#' `pkg[["name"]]` are returned so that uses can be captured. This metadata
#' is used for providing richer context for metrics; for example, communicating
#' recursive data dependencies during derivation.
#'
#' @note
#' As with any syntactic parsing of R code, it is trivially possible to
#' circumvent discovery. This process is intended to capture data use in common
#' developer workflows.
#'
#' @keywords internal
#' @noRd
discover_data_derive_deps <- function(expr) {
  if (is.function(expr)) {
    return(unlist(c(
      lapply(unname(formals(expr)), discover_data_derive_deps),
      discover_data_derive_deps(body(expr))
    )))
  }

  if (!is.call(expr)) {
    return(character(0L))
  }

  if (
    is.call(expr) &&
      is.symbol(expr[[1L]]) &&
      as.character(expr[[1L]]) %in% c("$", "[[") &&
      is.name(expr[[2L]]) &&
      as.character(expr[[2L]]) == "pkg"
  ) {
    return(as.character(expr[[3L]]))
  }

  unlist(lapply(expr, discover_data_derive_deps))
}
