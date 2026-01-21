#' @include impl_data.R

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
    # Use pkg$name from DESCRIPTION rather than resource@package
    # resource@path may be NA, so we try to find it
    pkg_name <- pkg$name

    # Find package path if not provided
    pkg_path <- resource@path
    if (is.na(pkg_path)) {
      pkg_path <- find.package(pkg_name)
    }

    # Get exports from NAMESPACE
    ns <- parseNamespaceFile(basename(pkg_path), dirname(pkg_path))
    exports <- ns$exports
    exported_count <- length(exports)

    # Get help database - extract lib.loc from pkg_path
    lib_loc <- dirname(pkg_path)
    rd_db <- tools::Rd_db(package = pkg_name, lib.loc = lib_loc)
    help_page_count <- length(rd_db)

    # For each of the help pages, get its tags
    rd_db_tags <- lapply(
      rd_db,
      function(rd) {
        vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
      }
    )

    # Build mapping of exports to help pages via aliases
    export_to_page <- list()
    for (i in seq_along(rd_db)) {
      page_name <- names(rd_db)[i]
      entries <- rd_db[[i]]
      entry_tags <- rd_db_tags[[i]]
      alias_indices <- which(entry_tags == "\\alias")

      aliases <- if (length(alias_indices) == 0) {
        character(0)
      } else {
        vapply(
          alias_indices,
          function(j) {
            paste(unlist(entries[[j]]), collapse = "")
          },
          character(1)
        )
      }

      for (alias in aliases) {
        if (alias %in% exports) {
          export_to_page[[alias]] <- c(export_to_page[[alias]], page_name)
        }
      }
    }

    # Find which exports have documentation
    documented_exports <- names(export_to_page)
    undocumented_exports <- setdiff(exports, documented_exports)

    # Find help pages with examples
    pages_with_examples <- names(rd_db)[
      vapply(
        rd_db_tags,
        function(rd_tags) {
          "\\examples" %in% rd_tags
        },
        logical(1)
      )
    ]

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
    round(info$documented_exports_count / info$exported_count, 3)
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
    round(runif(1, min = 0.5, max = 1.0), 3)
  }
)

impl_data(
  "exports_help_coverage",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Most well-maintained packages document all exports
    round(runif(1, min = 0.8, max = 1.0), 3)
  }
)
