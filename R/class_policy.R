#' Package Evaluation Policy Class
#'
#' A descriptor of how package resources should be discovered, indicating
#' which types of package resources should be considered and how they must be
#' used to produce consistently sourced information.
#'
#' The policy takes effect when packages are passed to [`pkg()`], limiting how
#' package resources can be discovered. A policy can be applied globally using
#' provided [`val.meter::options`].
#'
#' @usage
#' policy(
#'   accepted_resources = list(
#'     source_archive_resource,
#'     source_code_resource,
#'     install_resource
#'   ),
#'   source_resources = list(
#'     repo_resource
#'   ),
#'   permissions = class_permissions(FALSE)
#' )
#'
#' @examples
#' \dontrun{
#' # discover locally installed file path, create `pkg` from `local_resource`
#' pkg(find.package("val.meter"))
#'
#' # disable local resource discovery
#' options(val.meter.policy = policy(
#'   accepted_resources = list(source_archive_resource)
#' ))
#'
#' # expect error - unable to discover resource
#' tryCatch(
#'   pkg(find.package("val.meter")),
#'   error = function(error, ...) message(error$message)
#' )
#' }
#'
#' @export
#' @keywords safeguard
policy <- class_policy <- new_class(
  "policy",
  properties = list(
    #' @param accepted_resources A list of resources types to permit. Ordered by
    #'   priority, highest to lowest.
    accepted_resources = new_property(
      class_list,
      default = quote(list(
        source_archive_resource,
        source_code_resource,
        install_resource
      ))
    ),

    #' @param source_resources A list of additional resource types, which may be
    #'   used to discover a resource of an accepted type. For example, even if
    #'   only [`source_archive_resource()`]s are accepted, a [`repo_resource()`]
    #'   could be used as a means of acquiring the archive source, so long as it
    #'   can be [`S7::convert()`]ed into a [`source_archive_resource()`].
    source_resources = new_property(
      class_list,
      default = quote(list(cran_repo_resource, repo_resource))
    ),

    #' @param permissions Behavioral permissions provided for resource
    #'   acquisition. For example, downloading and installing source code for
    #'   more accurate metric evaluation requires the `"network"` and `"write"`
    #'   permissions.
    permissions = new_property(
      class_permissions,
      default = quote(class_permissions(FALSE)),
      setter = setter_try_from()
    )
  )
)
