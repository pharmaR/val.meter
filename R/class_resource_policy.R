#' Package Resource Policy Class
#'
#' A descriptor of how package resources should be discovered, indicating
#' which types of package resources should be considered and how they must be
#' used to produce consistently sourced information.
#'
#' @export
#' @name resource_policy
resource_policy <- class_resource_policy <- new_class(
  "resource_policy",
  properties = list(
    #' @field types A list of resources types to permit. Ordered by priority,
    #' highest to lowest.
    accepted_resources = new_property(
      class_list,
      default = list(
        source_archive_resource,
        source_code_resource,
        install_resource
      )
    ),

    #' @field source_resources A list of additional resource types,
    #' which may be used to discover a resource of an accepted type. For
    #' example, even if only [`archive_source_resource()`]s are accepted,
    #' a [`repo_resource()`] could be used as a means of acquiring the archive
    #' source, so long as it can be [`S7::convert()`]ed into a
    #' [`archive_source_resource()`].
    source_resources = new_property(
      class_list,
      default = list(
        repo_resource
      )
    ),

    #' @field Behavioral scopes provided for resource acquisition. For example,
    #' downloading and installing source code for more accurate metric
    #' evaluation requires the `"network"` and `"write"` scopes.
    scopes = new_property(
      class_permissions,
      default = permissions(FALSE)
    )
  )
)
