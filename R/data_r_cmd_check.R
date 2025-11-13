#' @include impl_data.R

impl_data(
  "r_cmd_check",
  class = S7::new_S3_class("rcmdcheck"),
  tags = c("execution"),
  suggests = "rcmdcheck",
  permissions = "execution"
)

impl_data(
  "r_cmd_check",
  for_resource = S7::new_union(
    local_source_resource,
    source_archive_resource
  ),
  function(pkg, resource, field, ..., quiet = opt("quiet")) {
    # suppress messages to avoid stdout output from subprocess
    # (eg warnings about latex availability not suppressed by rcmdcheck)

    wrapper <- if (quiet) {
      function(...) capture.output(..., type = "message")
    } else {
      identity
    }

    wrapper({
      result <- rcmdcheck::rcmdcheck(
        resource@path,
        quiet = quiet,
        error_on = "never",
        build_args = "--no-manual"
      )
    })

    result
  }
)

impl_data(
  "r_cmd_check_error_count",
  metric = TRUE,
  class = class_integer,
  tags = c("execution"),
  permissions = c(),
  title = "R CMD check Error Count",
  description = paste0(
    "the number of errors produced when running \\code{R CMD check}"
  ),
  function(pkg, resource, field, ...) {
    length(pkg$r_cmd_check$errors)
  }
)

impl_data(
  "r_cmd_check_error_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) rpois(1, 0.2)
)
