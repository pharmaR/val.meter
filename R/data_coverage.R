impl_data(
  "covr_coverage",
  class = S7::new_S3_class("coverage"),
  tags = c("execution"),
  suggests = "covr",
  permissions = "execution"
)

impl_data(
  "covr_coverage",
  for_resource = local_source_resource,
  function(pkg, resource, field, ...) {
    # package installs use `system2()` whose output cannot be captured by sink()
    # so we just execute quietly
    covr::package_coverage(resource@path, type = "tests", quiet = TRUE)
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
