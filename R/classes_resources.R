#' Package Resource Class
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
#' @name pkg_resource
pkg_resource <- new_class("pkg_resource")

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
pkg_source_resource <- new_class(
  "pkg_source_resource",
  parent = pkg_resource,
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
pkg_archive_source_resource <- new_class(
  "pkg_archive_source_resource",
  parent = pkg_source_resource,
)

#' Package Install Resource Class
#'
#' An installed version of a package, as would be found in a package library.
#'
#' @export
pkg_install_resource <- new_class(
  "pkg_install_resource",
  parent = pkg_source_resource
)

#' Package Local Resource Class Union
#'
#' A union of all package resource classes that have local source code.
#'
#' @export
pkg_local_resource <- new_union(
  pkg_source_resource,
  pkg_install_resource
)

#' Package Repository Resource Class
#'
#' A reference to a listing in an R package repository.
#'
#' @export
pkg_repo_resource <- new_class(
  "pkg_repo_resource",
  parent = pkg_resource,
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
pkg_git_resource <- new_class(
  "pkg_git_resource",
  parent = pkg_resource,
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
#' @name pkg_resource_policy
pkg_resource_policy <- new_class(
  "pkg_resource_policy",
  properties = list(
    #' @field types A list of resources types to permit. Ordered by priority,
    #' highest to lowest.
    types = new_property(
      class_list,
      default = list(
        pkg_source_resource,
        pkg_archive_source_resource,
        pkg_install_resource
      )
    ),

    #' @field accepted_source_types A list of additional resources types to
    #' use, ordered by priority, highest to lowest. These types must be able
    #' to be [`S7::convert()`]ed into one of `@types` to be used as a resource.
    accepted_source_types = new_property(
      class_list,
      default = list(
        pkg_repo_resource,
        pkg_git_resource
      )
    ),

    #' @field permit_download When `TRUE`, permits downloading of additional
    #' files. Most commonly, this means downloading the package archive from
    #' a provided repository.
    permit_download = new_property(class_logical, default = TRUE)
  )
)
