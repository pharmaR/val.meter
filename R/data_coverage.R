impl_data(
  "covr_coverage",
  class = S7::new_S3_class("coverage"),
  tags = c("execution"),
  suggests = c("covr", "callr"),
  permissions = "execution"
)

impl_data(
  "covr_coverage",
  for_resource = local_source_resource,
  function(pkg, resource, field, ...) {
    # covr installs the package and runs its tests using `system()` and
    # `system2()`, which can't be interrupted by a time limit (see `timeout`
    # option), so run it in a subprocess, which can. Their output can't be
    # captured by sink() either, so we just execute quietly.
    callr::r(
      function(path) {
        covr::package_coverage(path, type = "tests", quiet = TRUE)
      },
      args = list(path = resource@path),
      show = FALSE
    )
  }
)

impl_data(
  "test_expression_coverage_fraction",
  metric = TRUE,
  class = class_double,
  title = "Test coverage of package code by expression",
  description = paste0(
    "The fraction of expressions of package code that are evaluated by any ",
    "test"
  ),
  function(pkg, resource, field, ...) {
    tally <- covr::tally_coverage(pkg$covr_coverage, by = "expression")
    mean(tally$value > 0)
  }
)

impl_data(
  "test_expression_coverage_fraction",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    1 - pmin(pmax(rnorm(1, 0.5, 0.45)**2, 0), 1)
  }
)

impl_data(
  "test_line_coverage_fraction",
  metric = TRUE,
  class = class_double,
  title = "Test coverage of package code by line",
  description = paste0(
    "The fraction of lines of package code that are evaluated by any test"
  ),
  function(pkg, resource, field, ...) {
    tally <- covr::tally_coverage(pkg$covr_coverage, by = "line")
    mean(tally$value > 0)
  }
)

impl_data(
  "test_line_coverage_fraction",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    1 - pmin(pmax(rnorm(1, 0.5, 0.45)**2, 0), 1)
  }
)
