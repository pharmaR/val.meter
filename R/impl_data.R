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
#' @section Data versus metrics:
#'
#' Every value `r packageName()` derives about a package is *data*. A *metric*
#' is a specially-designated subset of that data. The two terms are often used
#' interchangeably, but the distinction matters when implementing new fields:
#'
#' \describe{
#'   \item{Data}{Any information that can be derived about a package, accessed
#'     with `pkg$field`. Data may be a raw, richly-structured or intermediate
#'     value used only to calculate other fields (for example, the parsed
#'     `DESCRIPTION` or a package's raw web page HTML). Data carries no
#'     constraints on its type.}
#'   \item{Metric}{Data that has additionally been declared a metric --
#'     structured, regular information intended for systematic,
#'     decision-making comparisons *across* packages (for example, a
#'     `R CMD check` error count or a download total). To keep metrics
#'     comparable, a metric must resolve to an *atomic* value (an
#'     `S7` `class_atomic` subclass); declaring a non-atomic field as a
#'     metric is an error.}
#' }
#'
#' A field becomes a metric when its metadata sets `metric = TRUE`, either
#' through the `metric` argument of [`impl_data()`]/[`impl_data_info()`] or via
#' `impl_metric()`. This flag is what [`metrics()`] uses to decide what to
#' surface: [`metrics()`] lists (or, given a [`pkg`], calculates) only metrics
#' by default, while `metrics(all = TRUE)` also returns the non-metric,
#' typically intermediate, data.
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
#' impl_data(
#'   "name_character_count",
#'   function(pkg, resource, field, ...) nchar(pkg$name)
#' )
#'
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
#' @param name `character(1L)` the data field name to implement.
#' @param fn `function` to use when deriving the data for a `pkg`.
#' @param for_resource `resource` classes that can use this method to derive
#'   data. Classes can also be `S7::new_union()`s or abstract classes to allow
#'   multiple resources to use the same method.
#' @param ... Additional arguments passed from `impl_data` to `impl_data_info`
#'   and are provided as metadata.
#' @param overwrite `logical(1L)` flag indicating that the method should
#'   overwrite an existing method that uses the same dispatch arguments.
#' @param quiet `logical(1L)` flag indicating that overwriting should be quiet,
#'   suppressing messages emitted during overwriting.
#'
#' @export
impl_data <- function(
  name,
  fn,
  for_resource = resource,
  ...,
  overwrite = FALSE,
  quiet = FALSE
) {
  dispatch_classes <- list(pkg_data_class(name), class_missing)
  is_info_impl <- is_implemented(pkg_data_info, dispatch_classes)

  if (!is_info_impl || ...length()) {
    impl_data_info(
      name = name,
      ...,
      overwrite = overwrite,
      quiet = quiet
    )
  }

  if (!missing(fn)) {
    impl_data_derive(
      name = name,
      fn = fn,
      for_resource = for_resource,
      overwrite = overwrite,
      quiet = quiet
    )
  }
}


#' @describeIn pkg_data
#' Associate metadata with the data field
#'
#' @param class `S7::S7_class` or `character(n)`. A return type that is type
#'   checked after method evaluation. `character` values will be coerced into
#'   `S7` representations of `S3` classes.
#' @param title `character(1L)` data title, used for user-facing communication
#'   about data derivation or purpose.
#' @param description `character(n)` or `Rd` object used as longer-form
#'   documentation about the data.
#' @param tags `tags()` associated with the data.
#' @param permissions `permissions()` required to compute the data.
#' @param suggests `character(n)` packages which must be installed in order to
#'   derive the data.
#' @param metric `logical(1L)` flag indicating whether the data should be
#'   user-facing as a metric. When `TRUE`, `class` must be [`atomic`].
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
  overwrite = FALSE,
  quiet = FALSE
) {
  tags <- convert(tags, class_tags)
  suggests <- convert(suggests, class_suggests)
  permissions <- convert(permissions, class_permissions)

  if (is.character(class)) {
    class <- S7::new_S3_class(class)
  }

  dispatch_classes <- list(pkg_data_class(name), class_missing)
  is_info_impl <- is_implemented(pkg_data_info, dispatch_classes)

  if (is_info_impl && !overwrite) {
    stop(
      "data info for '",
      name,
      "' is already implemented. Use ",
      "overwrite=TRUE to modify."
    )
  }

  data_class <- S7::as_class(class)
  info <- data_info(
    metric = metric,
    title = title,
    description = description,
    tags = tags,
    data_class = data_class,
    suggests = suggests,
    permissions = permissions
  )

  handler <- if (quiet) suppress_method_overwrite else identity
  handler({
    method(pkg_data_info, dispatch_classes) <-
      function(field, resource, ...) {
        info
      }
  })
}

#' @describeIn pkg_data
#' Register a derivation function for a data field and package resource.
#'
#' @include utils.R
#' @include utils_cli.R
#' @include utils_assertions.R
#' @export
impl_data_derive <- function(
  name,
  fn,
  for_resource,
  overwrite = FALSE,
  quiet = FALSE
) {
  dispatch_classes <- list(class_pkg, for_resource, pkg_data_class(name))
  is_derive_impl <- is_implemented(pkg_data_derive, dispatch_classes)
  if (is_derive_impl && !overwrite) {
    stop(fmt(
      "'{name}' data derive method already implemented for ",
      "{.cls {class_desc(resource)}}. Use overwrite=TRUE to replace."
    ))
  }

  handler <- if (quiet) suppress_method_overwrite else identity
  handler(method(pkg_data_derive, dispatch_classes) <- fn)
}
