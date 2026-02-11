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
#' @param installed_matrix Matrix of available packages from available.packages()
#' @param dependencies Character vector of dependency types
#' @return Integer count of reverse dependencies
#' @noRd
count_reverse_deps <- function(pkg_name, installed_matrix, 
                               dependencies = c("Depends", "Imports", "LinkingTo")) {
  # Use tools::dependsOnPkgs to find packages that depend on pkg_name
  revdeps <- tools::dependsOnPkgs(
    pkgs = pkg_name,
    dependencies = dependencies,
    recursive = FALSE,  # Direct dependencies only
    installed = installed_matrix
  )
  
  length(revdeps)
}

# Metric 1: CRAN reverse dependencies count
impl_data(
  "cran_reverse_dependencies_count",
  class = class_integer,
  metric = TRUE,
  tags = c("adoption", "transient"),
  permissions = c("network"),
  suggests = character(0),
  title = "CRAN Reverse Dependencies Count",
  
  description = paste(
    "The number of packages on \\acronym{CRAN} that directly depend on this package",
    "through \\code{Depends}, \\code{Imports}, or \\code{LinkingTo} fields.",
    "This metric reflects adoption within the CRAN ecosystem and indicates how many",
    "packages would be affected by breaking changes. Higher counts suggest wider usage",
    "and community trust, but also greater responsibility for maintaining backward compatibility."
  )
)

impl_data(
  "cran_reverse_dependencies_count",
  for_resource = cran_repo_resource,
  function(pkg, resource, field, ...) {
    pkg_name <- pkg$name
    
    # Fetch CRAN packages using available.packages()
    cran_matrix <- fetch_packages_matrix(repos = resource@repo)
    
    # Count reverse dependencies
    count_reverse_deps(
      pkg_name,
      cran_matrix,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
)

impl_data(
  "cran_reverse_dependencies_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Return realistic random count (most packages have few revdeps)
    sample(c(0L, 0L, 0L, 1L, 2L, 3L, 5L, 10L, 25L), 1L)
  }
)

# Metric 2: Bioconductor reverse dependencies count
impl_data(
  "bioc_reverse_dependencies_count",
  class = class_integer,
  metric = TRUE,
  tags = c("adoption", "transient"),
  permissions = c("network"),
  suggests = c("BiocManager"),
  title = "Bioconductor Reverse Dependencies Count",
  
  description = paste(
    "The number of packages on \\emph{Bioconductor} that directly depend on this package",
    "through \\code{Depends}, \\code{Imports}, or \\code{LinkingTo} fields.",
    "This metric is particularly relevant for packages in the bioinformatics and",
    "computational biology domains. For \\acronym{CRAN} packages, this counts",
    "Bioconductor packages that depend on them. Requires the \\code{BiocManager} package."
  )
)

# CRAN packages can be dependencies of Bioconductor packages
impl_data(
  "bioc_reverse_dependencies_count",
  for_resource = cran_repo_resource,
  function(pkg, resource, field, ...) {
    pkg_name <- pkg$name
    
    # Get Bioconductor software repository
    bioc_soft <- get_bioc_software_repo()
    
    if (length(bioc_soft) == 0) {
      stop("BiocManager package required for Bioconductor metrics")
    }
    
    # Fetch Bioconductor packages
    bioc_matrix <- fetch_packages_matrix(repos = bioc_soft)
    
    # Count reverse dependencies
    count_reverse_deps(
      pkg_name,
      bioc_matrix,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
)

# Bioconductor packages can have dependencies from both CRAN and Bioc
impl_data(
  "bioc_reverse_dependencies_count",
  for_resource = bioc_repo_resource,
  function(pkg, resource, field, ...) {
    pkg_name <- pkg$name
    
    # Fetch Bioconductor packages using the resource's repo
    bioc_matrix <- fetch_packages_matrix(repos = resource@repo)
    
    # Count reverse dependencies
    count_reverse_deps(
      pkg_name,
      bioc_matrix,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
)

impl_data(
  "bioc_reverse_dependencies_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Bioc packages often have more revdeps due to specialized domains
    sample(c(0L, 1L, 5L, 10L, 25L, 50L, 100L), 1L)
  }
)

# Metric 3: All repositories reverse dependencies count
impl_data(
  "repos_reverse_dependencies_count",
  class = class_integer,
  metric = TRUE,
  tags = c("adoption", "transient"),
  permissions = c("network"),
  suggests = c("BiocManager"),
  title = "All Repositories Reverse Dependencies Count",
  
  description = paste(
    "The total number of packages across \\acronym{CRAN} and \\emph{Bioconductor} that",
    "directly depend on this package through \\code{Depends}, \\code{Imports}, or",
    "\\code{LinkingTo} fields. This provides a comprehensive view of adoption across",
    "the entire R ecosystem. For \\acronym{CRAN} packages, this includes reverse",
    "dependencies from both \\acronym{CRAN} and \\emph{Bioconductor}. For",
    "\\emph{Bioconductor} packages, only \\emph{Bioconductor} reverse dependencies",
    "are counted (as Bioconductor packages cannot be strong dependencies of",
    "\\acronym{CRAN} packages). Requires the \\code{BiocManager} package."
  )
)

# For CRAN packages, count revdeps from both CRAN and Bioconductor
impl_data(
  "repos_reverse_dependencies_count",
  for_resource = cran_repo_resource,
  function(pkg, resource, field, ...) {
    pkg_name <- pkg$name
    
    # Fetch CRAN packages
    cran_matrix <- fetch_packages_matrix(repos = resource@repo)
    cran_count <- count_reverse_deps(
      pkg_name,
      cran_matrix,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
    
    # Try to get Bioconductor reverse dependencies if BiocManager is available
    bioc_count <- tryCatch({
      bioc_soft <- get_bioc_software_repo()
      
      if (length(bioc_soft) == 0) {
        return(0L)
      }
      
      bioc_matrix <- fetch_packages_matrix(repos = bioc_soft)
      count_reverse_deps(
        pkg_name,
        bioc_matrix,
        dependencies = c("Depends", "Imports", "LinkingTo")
      )
    }, error = function(e) 0L)
    
    cran_count + bioc_count
  }
)

# For Bioconductor packages, count only Bioc revdeps
# (CRAN packages cannot have strong deps on Bioc packages)
impl_data(
  "repos_reverse_dependencies_count",
  for_resource = bioc_repo_resource,
  function(pkg, resource, field, ...) {
    pkg_name <- pkg$name
    
    # Fetch Bioconductor packages
    bioc_matrix <- fetch_packages_matrix(repos = resource@repo)
    
    # Count reverse dependencies
    count_reverse_deps(
      pkg_name,
      bioc_matrix,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
)

impl_data(
  "repos_reverse_dependencies_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Combined repos should be sum of CRAN + Bioc (roughly)
    sample(c(0L, 1L, 2L, 5L, 10L, 25L, 50L, 100L, 200L), 1L)
  }
)
