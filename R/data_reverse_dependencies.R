#' @include impl_data.R

# Reverse dependencies metrics implementation

#' Fetch package database using available.packages()
#'
#' @param repos Character vector of repository URLs
#' @return Matrix of available packages with dependency information
#' @noRd
fetch_packages_matrix <- function(repos) {
  available.packages(repos = repos, filters = "duplicates")
}

#' Count reverse dependencies for a package
#'
#' @param pkg_name Character string, name of the package
#' @param installed_matrix Matrix of available packages from
#'   available.packages()
#' @param dependencies Character vector of dependency types
#' @return Character vector of package names that depend on pkg_name
#' @noRd
#' @importFrom tools dependsOnPkgs
get_reverse_deps <- function(
  pkg_name,
  installed_matrix,
  dependencies = c("Depends", "Imports", "LinkingTo")
) {
  dependsOnPkgs(
    pkgs = pkg_name,
    dependencies = dependencies,
    recursive = FALSE,  # Direct dependencies only
    installed = installed_matrix
  )
}

impl_data(
  "cran_reverse_dependencies",
  class = class_character,
  metric = FALSE,
  tags = c("adoption", "transient"),
  permissions = c("network"),
  title = "CRAN Reverse Dependencies",
  description = paste(
    "The names of packages on \\acronym{CRAN} that directly depend on",
    "this package through \\code{Depends}, \\code{Imports}, or",
    "\\code{LinkingTo} fields."
  )
)

impl_data(
  "cran_reverse_dependencies",
  for_resource = cran_repo_resource,
  function(pkg, resource, field, ...) {
    cran_matrix <- fetch_packages_matrix(repos = resource@repo)

    get_reverse_deps(
      pkg$name,
      cran_matrix,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
)

impl_data(
  "cran_reverse_dependencies",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Simulate reverse dependencies with a random sample of package names
    sample(
      paste0("mockpkg", seq_len(10)),
      size = min(rpois(1, 2), 10),
      replace = FALSE
    )
  }
)


impl_data(
  "cran_reverse_dependencies_count",
  class = class_integer,
  metric = TRUE,
  tags = c("adoption", "transient"),
  permissions = c(),
  title = "CRAN Reverse Dependencies Count",

  description = paste(
    "The number of packages on \\acronym{CRAN} that directly depend on",
    "this package through \\code{Depends}, \\code{Imports}, or",
    "\\code{LinkingTo} fields. This metric reflects adoption within the CRAN",
    "ecosystem and indicates how many packages would be affected by breaking",
    "changes. Higher counts suggest wider usage and community trust, but",
    "also greater responsibility for maintaining backward compatibility."
  )
)

impl_data(
  "cran_reverse_dependencies_count",
  for_resource = new_union(cran_repo_resource, mock_resource),
  function(pkg, resource, field, ...) {
    # Just count the length of rev dep vector
    length(pkg$cran_reverse_dependencies)
  }
)
