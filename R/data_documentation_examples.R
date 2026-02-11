#' @include impl_data.R

# Documentation examples analysis helpers

#' Extract aliases from an Rd object
#'
#' @param rd An Rd object from tools::Rd_db()
#' @return Character vector of aliases
#' @noRd
extract_rd_aliases <- function(rd) {
  tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
  alias_indices <- which(tags == "\\alias")

  if (length(alias_indices) == 0) {
    return(character(0))
  }

  vapply(
    alias_indices,
    function(i) paste(unlist(rd[[i]]), collapse = ""),
    character(1)
  )
}

#' Get tags for all help pages in an Rd database
#'
#' @param rd_db Help database from tools::Rd_db()
#' @return List of character vectors, one per help page
#' @noRd
get_rd_db_tags <- function(rd_db) {
  lapply(
    rd_db,
    function(rd) {
      vcapply(rd, function(x) attr(x, "Rd_tag"))
    }
  )
}

#' Build mapping of exports to help pages
#'
#' @param rd_db Help database from tools::Rd_db()
#' @param rd_db_tags List of tags for each help page
#' @param exports Character vector of package exports
#' @return Named list mapping export names to help page names
#' @noRd
map_exports_to_pages <- function(rd_db, rd_db_tags, exports) {
  export_to_page <- list()

  for (i in seq_along(rd_db)) {
    page_name <- names(rd_db)[[i]]
    aliases <- extract_rd_aliases(rd_db[[i]])

    for (alias in aliases) {
      if (alias %in% exports) {
        export_to_page[[alias]] <- c(export_to_page[[alias]], page_name)
      }
    }
  }

  export_to_page
}

#' Find help pages that contain examples
#'
#' @param rd_db Help database from tools::Rd_db()
#' @param rd_db_tags List of tags for each help page
#' @return Character vector of help page names with examples
#' @noRd
find_pages_with_examples <- function(rd_db, rd_db_tags) {
  has_examples <- vlapply(
    rd_db_tags,
    function(tags) "\\examples" %in% tags
  )

  names(rd_db)[has_examples]
}

#' Analyze documentation examples for a package
#'
#' @param pkg_name Package name
#' @param pkg_path Path to installed package (or NA to auto-detect)
#' @return List with documentation statistics
#' @noRd
analyze_documentation_examples <- function(pkg_name, pkg_path = find.package(pkg_name)) {

  # Get exports from NAMESPACE
  ns <- parseNamespaceFile(basename(pkg_path), dirname(pkg_path))
  exports <- ns$exports
  exported_count <- length(exports)

  # Get help database
  lib_loc <- dirname(pkg_path)
  rd_db <- tools::Rd_db(package = pkg_name, lib.loc = lib_loc)
  help_page_count <- length(rd_db)

  # Get tags for all help pages
  rd_db_tags <- get_rd_db_tags(rd_db)

  # Build export-to-page mapping
  export_to_page <- map_exports_to_pages(rd_db, rd_db_tags, exports)

  # Find documented and undocumented exports
  documented_exports <- names(export_to_page)
  undocumented_exports <- setdiff(exports, documented_exports)

  # Find pages with examples
  pages_with_examples <- find_pages_with_examples(rd_db, rd_db_tags)

  # Return documentation stats
  list(
    exported_count = exported_count,
    help_page_count = help_page_count,
    help_pages_with_examples_count = length(pages_with_examples),
    documented_exports_count = length(documented_exports),
    undocumented_exports = undocumented_exports,
    help_pages_with_examples = pages_with_examples
  )
}

impl_data(
  "documentation_examples",
  class = class_any,
  title = "Documentation Examples Information",
  description = paste(
    "Analysis of help pages and examples for a package.",
    "Includes counts of exported objects, help pages, and pages with",
    "\\code{\\\\examples} sections. Also identifies undocumented exports."
  ),
  for_resource = install_resource,
  function(pkg, resource, field, ...) {
    analyze_documentation_examples(pkg$name, resource@path)
  }
)

impl_data(
  "help_pages_with_examples_count",
  class = class_integer,
  metric = TRUE,
  tags = c("best practice"),
  permissions = c(),
  title = "Help Pages with Examples Count",
  description = paste(
    "The number of help pages that include an \\code{\\\\examples} section.",
    "This provides an absolute count of how many documentation pages",
    "demonstrate the package functionality with example code."
  ),
  function(pkg, resource, field, ...) {
    as.integer(pkg$documentation_examples$help_pages_with_examples_count)
  }
)

impl_data(
  "help_examples_coverage",
  class = class_double,
  metric = TRUE,
  tags = c("best practice"),
  permissions = c(),
  title = "Help Pages Examples Coverage",
  description = paste(
    "The fraction of help pages that include an \\code{\\\\examples} section.",
    "Calculated as the number of help pages with examples divided by the",
    "total number of help pages. Values range from 0.0 (no examples) to",
    "1.0 (all pages have examples)."
  ),
  function(pkg, resource, field, ...) {
    info <- pkg$documentation_examples
    if (info$help_page_count == 0) return(NA_real_)
    round(info$help_pages_with_examples_count / info$help_page_count, 3)
  }
)

impl_data(
  "exports_help_coverage",
  class = class_double,
  metric = TRUE,
  tags = c("best practice"),
  permissions = c(),
  title = "Exports Help Coverage",
  description = paste(
    "The fraction of exported objects that have help pages.",
    "Calculated by matching package exports against \\code{\\\\alias} tags",
    "in help pages. Values range from 0.0 (no exports documented) to",
    "1.0 (all exports have help pages)."
  ),
  function(pkg, resource, field, ...) {
    info <- pkg$documentation_examples
    if (info$exported_count == 0) return(NA_real_)
    info$documented_exports_count / info$exported_count
  }
)

# Mock implementations for random packages
impl_data(
  "help_pages_with_examples_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Random count between 5 and 50
    as.integer(sample(5:50, 1))
  }
)

impl_data(
  "help_examples_coverage",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Random coverage between 0.5 and 1.0 (most packages have decent coverage)
    runif(1, min = 0.5, max = 1.0)
  }
)

impl_data(
  "exports_help_coverage",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Most well-maintained packages document all exports
    runif(1, min = 0.8, max = 1.0)
  }
)
