#' Source Control Metrics
#'
#' Metrics for assessing whether a package has source code on a recognized
#' hosting platform based on an allow-list of known domains.
#'
#' @section Metrics:
#'
#' \describe{
#'   \item{`recognized_source_url`}{The inferred URL of the package's source
#'     control repository, extracted from the URL and BugReports fields in the
#'     DESCRIPTION file and matched against an allow-list of recognized domains.}
#'   \item{`has_recognized_source`}{A logical indicating whether the package has
#'     a source code repository on a recognized hosting platform from the
#'     allow-list.}
#' }
#'
#' @section Allow-List Approach:
#'
#' These metrics use an allow-list of recognized source control hosting domains
#' to identify source repositories. This means:
#' \itemize{
#'   \item Only URLs matching domains in the allow-list are recognized
#'   \item Packages may have source control not detected if hosted elsewhere
#'   \item The allow-list is customizable to include additional domains
#'   \item Results indicate recognized hosting, not definitive source control presence
#' }
#'
#' @section Customization:
#'
#' The recognized domains can be customized. For details on the default
#' recognized domains and how to customize them, see [`options`].
#'
#' This is useful for:
#' \itemize{
#'   \item Adding self-hosted GitLab, Gitea, or Forgejo instances
#'   \item Including internal enterprise git hosting services
#'   \item Supporting additional federated git providers
#' }
#'
#' @examples
#' \dontrun{
#' # Check if a package has source on a recognized platform
#' p <- pkg("ggplot2")
#' p$has_recognized_source  # TRUE
#' p$recognized_source_url  # "https://github.com/tidyverse/ggplot2"
#'
#' # Customize to add self-hosted GitLab to the allow-list
#' # Save the original value to restore later
#' old_domains <- opt_set("source_control_domains", c(
#'   "github.com",
#'   "gitlab.com",
#'   "git.mycompany.com"
#' ))
#' 
#' # Or extend the defaults
#' default_domains <- opt("source_control_domains")
#' old_domains <- opt_set("source_control_domains", c(
#'   default_domains,
#'   "git.mycompany.com",
#'   "forge.myorg.net"
#' ))
#' 
#' # Restore original value
#' opt_set("source_control_domains", old_domains)
#' }
#'
#' @seealso [`options`] for details on the `source_control_domains` option
#' @name source_control_metrics
#' @include impl_data.R
NULL

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
