#' @include impl_data.R

# Robustness lints metric implementation

# Auxiliary data: full lint results
impl_data(
  "robustness_lints",
  class = S7::new_S3_class("lints"),
  tags = c("execution"),
  suggests = "lintr",
  permissions = "execution",
)

impl_data(
  "robustness_lints",
  for_resource = source_code_resource,
  function(pkg, resource, field, ..., quiet = opt("quiet")) {
    wrapper <- if (quiet) {
      function(...) capture.output(..., type = "message")
    } else {
      identity
    }
    
    wrapper({
      # Suppress lintr messages to avoid stdout output from subprocess
      result <- lintr::lint_package(
        path = resource@path,
        linters = lintr::linters_with_tags("robustness"),
        parse_settings = FALSE  # Don't read .lintr config for consistency
      )
    })

    result
  }
)

# Primary metric: count of robustness lints
impl_data(
  "robustness_lint_count",
  metric = TRUE,
  class = class_integer,
  tags = tags("best practice"),
  title = "Robustness Lint Count",
  description = paste(
    "Count of robustness lints identified by \\pkg{lintr}.",
    "Robustness lints identify potentially error-prone code patterns",
    "based on relatively unopinionated good programming practices.",
    "Lower counts indicate more robust code.",
    "See \\url{https://lintr.r-lib.org/reference/robustness_linters.html}"
  )
)

impl_data(
  "robustness_lint_count",
  for_resource = source_code_resource,
  function(pkg, resource, field, ...) {
    length(pkg$robustness_lints)
  }
)

impl_data(
  "robustness_lint_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) rpois(1, 10)
)
