#' @include impl_data.R

# BioConductor reverse dependencies metrics implementation
# Reuses the viable_revdep_packages() and get_reverse_deps() helpers defined
# in data_reverse_dependencies.R (both are called at runtime, so no load-time
# ordering dependency exists between the two files).

#' Known BioConductor repository URLs
#'
#' Returns the standard BioConductor repository URLs used to build the package
#' universe for reverse dependency analysis. Unlike \acronym{CRAN}, the
#' BioConductor package universe is spread across several repositories
#' (software, annotation, experiment data and workflows), so all are queried.
#'
#' The `"release"` alias always resolves to the current BioConductor release, so
#' the metric does not need to track the BioConductor version explicitly. A
#' different release (or an internal mirror) can be supplied through the
#' `val.meter.bioc_repos` option.
#'
#' @param version BioConductor release to target. Defaults to `"release"`.
#' @return Character vector of repository URLs.
#' @noRd
get_bioc_repos <- function(version = "release") {
  repos <- getOption("val.meter.bioc_repos")
  if (!is.null(repos)) {
    return(repos)
  }

  file.path(
    "https://bioconductor.org/packages",
    version,
    c("bioc", "data/annotation", "data/experiment", "workflows")
  )
}

impl_data(
  "bioc_reverse_dependencies",
  class = class_character,
  metric = FALSE,
  tags = c("adoption", "transient"),
  permissions = c("network"),
  title = "BioConductor Reverse Dependencies",
  description = paste(
    "The names of packages on \\acronym{BioConductor} that directly depend on",
    "this package through \\code{Depends}, \\code{Imports}, or",
    "\\code{LinkingTo} fields."
  )
)

impl_data(
  "bioc_reverse_dependencies",
  for_resource = cran_repo_resource,
  function(pkg, resource, field, ...) {
    # the BioConductor package universe is independent of the package's own
    # resource, so it is queried from the known BioConductor repositories
    # rather than from resource@repo.
    bioc_matrix <- viable_revdep_packages(repos = get_bioc_repos())

    get_reverse_deps(
      pkg$name,
      bioc_matrix,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
)

impl_data(
  "bioc_reverse_dependencies",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Simulate reverse dependencies with a random sample of package names
    sample(
      paste0("mockbiocpkg", seq_len(10)),
      size = min(rpois(1, 2), 10),
      replace = FALSE
    )
  }
)


impl_data(
  "bioc_reverse_dependencies_count",
  class = class_integer,
  metric = TRUE,
  tags = c("adoption", "transient"),
  permissions = c(),
  title = "BioConductor Reverse Dependencies Count",

  description = paste(
    "The number of packages on \\acronym{BioConductor} that directly depend",
    "on this package through \\code{Depends}, \\code{Imports}, or",
    "\\code{LinkingTo} fields. This metric reflects adoption within the",
    "BioConductor ecosystem and indicates how many packages would be affected",
    "by breaking changes. Higher counts suggest wider usage and community",
    "trust, but also greater responsibility for maintaining backward",
    "compatibility."
  )
)

impl_data(
  "bioc_reverse_dependencies_count",
  for_resource = new_union(cran_repo_resource, mock_resource),
  function(pkg, resource, field, ...) {
    # Just count the length of rev dep vector
    length(pkg$bioc_reverse_dependencies)
  }
)
