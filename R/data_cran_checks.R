#' @include impl_data.R

#' A memoised wrapper around `tools::CRAN_check_results`
#' @keywords internal
#' @importFrom tools CRAN_check_results
cran_check_results <- local({
  self <- environment()
  value <- NULL

  function() {
    if (is.null(value)) {
      self$value <- tools::CRAN_check_results()
    }

    self$value
  }
})

#' Fraction of CRAN status checks with status
#'
#' Returns the decimal fraction of CRAN status checks with the given `status`,
#' or `NA` if checks has no entries.
#'
#' @keywords internal
pkg_status_fraction <- function(checks, status) {
  if (length(checks) == 0L) {
    return(NA_real_)
  }

  sum(checks == status) / length(checks)
}

#' Helper to keep docs consistent
#' @keywords internal
pkg_status_desc <- function(status) {
  paste0(
    "The fraction of \\abbr{CRAN} checks that produced ",
    "\\code{",
    status,
    "}s. Note that only the current latest version of the ",
    "package on \\abbr{CRAN} has the check status available. By default, ",
    "See `?options` to include \\abbr{CRAN} status results regardless of ",
    "version."
  )
}

#' @importFrom options opt
impl_data(
  "cran_status",
  class = class_factor,
  permissions = "network",
  function(pkg, resource, field, ...) {
    any_ver <- options::opt("metrics.cran_status.ignore_version_mismatch")

    checks <- subset(
      cran_check_results(),
      Package == pkg$name & (Version == pkg$version | any_ver)
    )

    pkg_checks <- checks$Status
    names(pkg_checks) <- checks$Flavor

    pkg_checks
  }
)

impl_data(
  "cran_status_note_fraction",
  class = class_double,
  tags = c("transient", "execution"),
  metric = TRUE,
  title = "CRAN check NOTEs fraction by system flavor",
  description = pkg_status_desc("NOTE"),
  function(pkg, resource, field, ...) {
    pkg_status_fraction(pkg$cran_status, "NOTE")
  }
)

impl_data(
  "cran_status_warning_fraction",
  class = class_double,
  tags = c("transient", "execution"),
  metric = TRUE,
  title = "CRAN check WARNINGs fraction by system flavor",
  description = pkg_status_desc("WARNING"),
  function(pkg, resource, field, ...) {
    pkg_status_fraction(pkg$cran_status, "WARNING")
  }
)

impl_data(
  "cran_status_error_fraction",
  class = class_double,
  tags = c("transient", "execution"),
  metric = TRUE,
  title = "CRAN check ERRORs fraction by system flavor",
  description = pkg_status_desc("ERROR"),
  function(pkg, resource, field, ...) {
    pkg_status_fraction(pkg$cran_status, "ERROR")
  }
)

impl_data(
  "cran_status_failure_fraction",
  class = class_double,
  tags = c("transient", "execution"),
  metric = TRUE,
  title = "CRAN check FAILUREs fraction by system flavor",
  description = pkg_status_desc("FAILURE"),
  function(pkg, resource, field, ...) {
    pkg_status_fraction(pkg$cran_status, "FAILURE")
  }
)
