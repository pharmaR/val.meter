#' @include impl_data.R

# Source control URL extraction and inference
impl_data(
  "recognized_source_url",
  class = class_character,
  title = "Recognized Source Control URL",
  description = paste(
    "The \\acronym{URL} of the package's source control repository on a",
    "recognized hosting platform, determined by matching against an",
    "allow-list of known domains. Extracted from the \\acronym{URL} and",
    "\\code{BugReports} fields in the \\code{DESCRIPTION} file. The",
    "allow-list can be customized; see \\code{?options} for details."
  ),
  function(pkg, resource, field, ...) {
    # Get URLs from DESCRIPTION file
    desc_urls <- tryCatch(
      pkg$desc$get_urls(),
      error = function(e) character(0)
    )

    # Get BugReports field if available
    bug_reports <- tryCatch(
      pkg$desc$get_field("BugReports"),
      error = function(e) character(0)
    )

    # Combine all URLs to check, filtering out empty strings and NAs
    all_urls <- c(desc_urls, bug_reports)
    all_urls <- all_urls[!is.na(all_urls) & nzchar(all_urls)]

    if (length(all_urls) == 0) {
      return(character(0))
    }

    # Get recognized source control domains from options
    source_control_domains <- opt("source_control_domains")

    # Find all URLs matching known source control domains
    # We use case-insensitive matching by converting to lowercase
    # and fixed = TRUE to avoid regex special character issues (e.g., '.')
    is_src_url <- vapply(
      tolower(source_control_domains),
      grepl,
      logical(length(all_urls)),
      x = tolower(all_urls),
      fixed = TRUE
    )

    all_urls[rowSums(is_src_url) > 0]
  }
)

impl_data(
  "has_recognized_source",
  class = class_logical,
  metric = TRUE,
  tags = c("best practice"),
  permissions = c(),
  title = "Has Recognized Source Repository",
  description = paste(
    "Indicates whether the package has a source code repository on a",
    "recognized hosting platform from an allow-list of known domains.",
    "Inferred from the \\acronym{URL} and \\code{BugReports} fields in the",
    "\\code{DESCRIPTION} file. See \\code{?options} for customizing the",
    "allow-list."
  ),
  function(pkg, resource, field, ...) {
    length(pkg$recognized_source_url) > 0L
  }
)

# Mock implementation for random packages
impl_data(
  "has_recognized_source",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Simulate realistic distribution - most packages have recognized source
    runif(1) > 0.2
  }
)
