#' @include impl_data.R

# Source control URL extraction and inference
impl_data(
  "recognized_source_url",
  class = class_character,
  title = "Recognized Source Control URL",
  description = paste(
    "The URL of the package's source control repository on a recognized",
    "hosting platform, determined by matching against an allow-list of",
    "known domains. Extracted from the URL and BugReports fields in the",
    "DESCRIPTION file. The allow-list can be customized; see ?options for",
    "details."
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
      return(NA_character_)
    }

    # Get recognized source control domains from options
    source_control_domains <- opt("source_control_domains")

    # Try to find a URL matching known source control domains
    # We use case-insensitive matching for domains
    for (url in all_urls) {
      for (domain in source_control_domains) {
        if (grepl(domain, url, ignore.case = TRUE)) {
          return(url)
        }
      }
    }

    # No known source control URL found
    NA_character_
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
    "Inferred from the URL and BugReports fields in the DESCRIPTION file.",
    "See ?options for customizing the allow-list."
  ),
  function(pkg, resource, field, ...) {
    !is.na(pkg$recognized_source_url)
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
