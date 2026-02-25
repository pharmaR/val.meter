#' @include impl_data.R

# Correctness lints metric implementation

# Auxiliary data: full lint results
impl_data(
  "correctness_lints",
  class = S7::new_S3_class("lints"),
  metric = FALSE, # Explicitly mark as auxiliary data, not a metric
  tags = c("execution"),
  suggests = "lintr",
  permissions = "execution",
  title = "Correctness Lint Results",
  description = paste(
    "Full \\pkg{lintr} correctness lint results for the package.",
    "Contains detailed information about each lint including line numbers,",
    "column positions, and lint messages. This is auxiliary data used to",
    "derive the \\code{correctness_lint_count} metric."
  )
)

impl_data(
  "correctness_lints",
  for_resource = source_code_resource,
  function(pkg, resource, field, ..., quiet = opt("quiet")) {
    wrapper <- if (quiet) {
      function(...) capture.output(..., type = "message")
    } else {
      identity
    }

    wrapper({
      result <- lintr::lint_package(
        path = resource@path,
        linters = lintr::linters_with_tags(
          tags = c("correctness", "common_mistakes", "robustness")
        ),
        parse_settings = FALSE # Don't read .lintr config for consistency
      )
    })

    result
  }
)

# Primary metric: count of correctness lints
impl_data(
  "correctness_lint_count",
  metric = TRUE,
  class = class_integer,
  tags = c("best practice"),
  title = "Correctness Lint Count",
  description = paste(
    "Count of correctness lints identified by \\pkg{lintr}.",
    "Combines linters from the \\emph{correctness}, \\emph{common_mistakes},",
    "and \\emph{robustness} tags to identify potentially error-prone code",
    "patterns and common programming mistakes.",
    "Lower counts indicate higher code quality.",
    "See \\url{https://lintr.r-lib.org/reference/correctness_linters.html}"
  )
)

impl_data(
  "correctness_lint_count",
  for_resource = source_code_resource,
  function(pkg, resource, field, ...) {
    length(pkg$correctness_lints)
  }
)

impl_data(
  "correctness_lint_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) rpois(1, 10)
)
