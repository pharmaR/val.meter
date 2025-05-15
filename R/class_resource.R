#' Resource Class
#'
#' A package resource is a resource for producing package information. Package
#' resources can vary from source code repositories to R package repository
#' listings. Each resources should be able to produce a downloadable version
#' of executable package code, but the extent of included code might vary
#' depending on source.
#'
#' Package resources should also implement `convert()`, providing a method for
#' converting into a more desirable source of information. For example, given
#' only a listing of a package in a repository, calling `convert()` on such
#' a reference may populate a local directory with the source code of the
#' package.
#'
#' @export
#' @name resource
resource <- class_resource <- new_class("resource", abstract = TRUE)

multi_resource <- new_class(
  "multi_resource",
  parent = resource,
  properties = list(
    resources = new_property(
      class_list,
      validator = function(value) {
        if (!any(vlapply(value, Negate(S7_inherits), resource)))
          "can only be constructed from a list of resources"
      }
    )
  )
)

#' Package Source Code Resource Class
#'
#' Note that in some cases, this is distinct from the source code extracted
#' from a package archive, as some files are sometimes excluded from package
#' builds (uploaded to repositories). This can include additional development-
#' related metadata files, but can sometimes include additional tests that take
#' too long to run by automated systems by the repository host.
#'
#' Commonly, this may mean omitting tests to avoid failures on CRAN's automated
#' build systems.
#'
#' @export
source_resource <- new_class(
  "source_resource",
  parent = resource,
  properties = list(
    path = new_property(
      class_character,
      default = NA_character_,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || !file.exists(value))
          "invalid path"
      }
    )
  )
)

#' Package Archive Source Code Resource Class
#'
#' The extracted source code from a package's build archive.
#'
#' @export
archive_source_resource <- new_class(
  "archive_source_resource",
  parent = source_resource,
)

#' Package Install Resource Class
#'
#' An installed version of a package, as would be found in a package library.
#'
#' @export
install_resource <- new_class(
  "install_resource",
  parent = source_resource
)

#' Package Local Resource Class Union
#'
#' A union of all package resource classes that have local source code.
#'
#' @export
local_resource <- new_union(source_resource, install_resource)

#' Package Repository Resource Class
#'
#' A reference to a listing in an R package repository.
#'
#' @export
repo_resource <- new_class(
  "repo_resource",
  parent = resource,
  properties = list(
    package = class_character,
    version = class_character,
    repo = class_character
  )
)

#' Package `git` Resource Class
#'
#' A reference to a listing in an R package git source code repository.
#'
#' @export
git_resource <- new_class(
  "git_resource",
  parent = resource,
  properties = list(
    http_url = class_character
  )
)

#' Package Resource Policy Class
#'
#' A descriptor of how package resources should be discovered, indicating
#' which types of package resources should be considered and how they must be
#' used to produce consistently sourced information.
#'
#' @export
#' @name resource_policy
resource_policy <- new_class(
  "resource_policy",
  properties = list(
    #' @field types A list of resources types to permit. Ordered by priority,
    #' highest to lowest.
    types = new_property(
      class_list,
      default = list(
        source_resource,
        archive_source_resource,
        install_resource
      )
    ),

    #' @field accepted_source_types A list of additional resources types to
    #' use, ordered by priority, highest to lowest. These types must be able
    #' to be [`S7::convert()`]ed into one of `@types` to be used as a resource.
    accepted_source_types = new_property(
      class_list,
      default = list(
        repo_resource,
        git_resource
      )
    ),

    #' @field permit_download When `TRUE`, permits downloading of additional
    #' files. Most commonly, this means downloading the package archive from
    #' a provided repository.
    permit_download = new_property(class_logical, default = TRUE)
  )
)

#' [`resource`] from `character`
#'
#' Attempt to find any suitable package sources.
#'
#' @noRd
method(convert, list(class_character, class_resource)) <-
  function(from, to, ..., policy = opt("policy")) {
    all_resource_types <- append(policy@types, policy@accepted_source_types)

    # create an empty list to populate with discovered resources
    resources <- list()
    length(resources) <- length(all_resource_types)

    # helper function to add a resource to our discovered resource list
    add_resource <- function(resource) {
      idx <- match(list(.s7_class(resource)), all_resource_types)
      if (is.na(idx) || !is.null(resources[[idx]])) return()
      resources[[idx]] <<- resource
    }

    # try to cast into each resource type
    for (resource_type in all_resource_types) {
      tryCatch(
        add_resource(convert(from, resource_type)),
        error = identity
      )
    }

    # filter out undiscovered resource types
    resources <- utils::head(resources, length(policy@types))
    resources <- Filter(Negate(is.null), resources)

    if (length(resources) < 1L) {
      stop("Unable to discover package resource")
    }

    if (length(resources) == 1L) {
      resources[[1L]]
    } else {
      multi_resource(resources)
    }
  }

method(convert, list(class_character, install_resource)) <-
  function(from, to) {
    if (file.exists(from) && file.exists(file.path(from, "INDEX"))) {
      return(to(path = from))
    } else if (length(paths <- find.package(from, quiet = TRUE)) > 0L) {
      return(convert(paths[[1]], to))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_character, archive_source_resource)) <-
  function(from, to) {
    if (file.exists(from) && file.exists(file.path(from, "MD5"))) {
      return(to(path = from))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_character, source_resource)) <-
  function(from, to) {
    if (
      file.exists(from) &&
        !file.exists(file.path(from, "INDEX")) &&
        !file.exists(file.path(from, "MD5")) &&
        file.exists(file.path(from, "DESCRIPTION"))
    ) {
      return(to(path = from))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_character, repo_resource)) <-
  function(from, to) {
    ap <- available.packages()
    ap_idx <- Position(function(pkg) identical(from, pkg), ap[, "Package"])

    if (!is.na(ap_idx)) {
      return(to(
        package = ap[ap_idx, "Package"],
        version = ap[ap_idx, "Version"],
        repo = ap[ap_idx, "Repository"]
      ))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }
