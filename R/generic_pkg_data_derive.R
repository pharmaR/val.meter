#' Derive a package data field
#'
#' Derive a [`pkg`] data field, giving a function by which a piece of package
#' data is calculated. This function is not called directly, it used by the
#' indexing functions implemented for [`pkg`] to populate new data fields.
#' Default implementations accept `field` as a `character`, automatically
#' dispatching on the field as a class object, and accept a missing
#' `resource`, instead iterating through `pkg` resources by priority.
#'
#' This function is used internally when accessing a [`pkg`] object using `$`,
#' `[[` and `[`.
#'
#' @param pkg A [`pkg()`]
#' @param resource A [`resource()`], or if not provided, the [`resource`]
#'   extracted from `pkg@resource`.
#' @param field Used for dispatching on which field to derive. Methods are
#'   provided such that a simple `character` field name can be passed and
#'   used to build a class for dispatching to the right derivation function.
#' @param ... Used by specific methods.
#'
#' @returns The derived field value.
#'
#' @family generics
#' @export
pkg_data_derive <- new_generic("pkg_data_derive", c("pkg", "resource", "field"))

#' Derive data and capture output
#'
#' Uses `evaluate::evaluate` to capture execution logs.
#'
#' @inheritParams pkg_data_derive
#'
#' @keywords internal
capture_pkg_data_derive <- function(
  pkg,
  resource,
  field,
  ...,
  quiet = opt("quiet")
) {
  # build a prettier call that will be output by evaluate() when not quiet
  x <- pkg
  pkg <- list(function() pkg_data_derive(pkg = x, field = field))
  names(pkg) <- field
  evaluate_fn <- function() {}
  body(evaluate_fn) <- as.call(list(call("$", as.symbol("pkg"), field)))

  # format output for a standard console width; force capture of ansi
  original_opts <- options(
    width = 80L,
    crayon.enabled = TRUE,
    cli.ansi = TRUE,
    cli.dynamic = FALSE,
    cli.num_colors = 256L
  )

  on.exit(options(original_opts))

  capture <- evaluate::evaluate(
    evaluate_fn,
    stop_on_error = 1L,
    debug = !isTRUE(quiet),
    output_handler = evaluate::new_output_handler(value = identity)
  )

  list(
    # omit code echo and return value
    logs = capture[-c(1, length(capture))],
    # just the return value
    data = capture[[length(capture)]]
  )
}

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
  function(pkg, resource, field, ..., field_name) {
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

#' Derive by iterating through a multi-resource
#'
#' Multi-resources are a unique resource type that allows deriving fields from
#' any number of resources. They are treated as a priority ordering of
#' resources. The available resources are iterated through sequentially and the
#' first resource that provides a value without error is returned.
#'
#' @noRd
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
#' @examples
#' mocked_resource <- mock_resource(package = "fakepkg", version = "1.2.3")
#' p <- pkg(mocked_resource, permissions(TRUE))
#'
#' # data which can be derived is still derived from existing mock data
#' pkg_data_derive(p, field = "name")
#'
#' # otherwise, it is generated on demand
#' pkg_data_derive(p, field = "downloads_total")
#'
#' @noRd
method(
  pkg_data_derive,
  list(class_pkg, class_mock_resource, pkg_data_class())
) <-
  function(pkg, resource, field, ...) {
    info <- pkg_data_info(field)
    tryCatch(
      # first try to evaluate as though this is a proper package, leveraging
      # real derivation rules for data that is interrelated
      pkg_data_derive(pkg, convert(resource, unknown_resource), field, ...),

      # should that fail, generate using mocking rules
      error = function(e, ...) {
        tryCatch(
          # try to derive a default mock value
          pkg_data_derive(
            pkg,
            resource,
            info@data_class,
            ...,
            field_name = field
          ),

          # should that error, return as-is
          error = identity
        )
      }
    )
  }

#' Derive for Mock Resource with class type
#'
#' When not implemented specifically, fall back to these generic generators of
#' random data given the preferred value type.
#'
#' @examples
#' p <- pkg(mock_resource())
#' pkg_data_derive(p, field = class_logical)
#'
#' # simulate a set of random logical data and tally results
#' table(vapply(
#'   1:100,
#'   function(...) pkg_data_derive(p, field = class_logical),
#'   logical(1L)
#' ))
#'
#' @noRd
method(
  pkg_data_derive,
  list(
    class_any,
    class_mock_resource,
    new_union(.s7_class, .s7_base_class, .s7_s3_class, .s7_any)
  )
) <-
  function(pkg, resource, field, ..., field_name = field) {
    pkg_data_derive(
      pkg = pkg,
      resource = resource,
      field = structure(
        list(),
        class = c(paste0("mock_", c(field$class, "data")), field$class)
      ),
      ...,
      field_name = field_name
    )
  }

#' Derive for Mock Resource for mocked base classes
#'
#' Eventually, mocked data falls back to generics implemented for special
#' `mock_` versions of base classes.
#'
#' @examples
#' p <- pkg(mock_resource())
#' derive_field <- structure(list(), class = "mock_integer")
#' pkg_data_derive(p, field = derive_field)
#'
#' @noRd
#'
method(
  pkg_data_derive,
  list(class_any, class_mock_resource, new_S3_class("mock_data"))
) <-
  function(pkg, resource, field, ...) NA

method(
  pkg_data_derive,
  list(class_any, class_mock_resource, new_S3_class("mock_logical"))
) <-
  function(pkg, resource, field, ...) sample(c(TRUE, FALSE), 1)

method(
  pkg_data_derive,
  list(class_any, class_mock_resource, new_S3_class("mock_integer"))
) <-
  function(pkg, resource, field, ...) rpois(n = 1, lambda = 2)

method(
  pkg_data_derive,
  list(class_any, class_mock_resource, new_S3_class("mock_character"))
) <-
  function(pkg, resource, field, ...) {
    paste(collapse = "", sample(letters, 24L, replace = TRUE))
  }

method(
  pkg_data_derive,
  list(class_any, class_mock_resource, new_S3_class("mock_numeric"))
) <-
  function(pkg, resource, field, ...) runif()^rpois(n = 1, lambda = 2)
