#' @include impl_data.R

impl_data(
  "covr_test_coverage",
  class = S7::new_S3_class("coverage"),
  tags = c("execution"),
  suggests = "covr",
  permissions = "execution"
)

impl_data(
  "covr_test_coverage",
  for_resource = local_source_resource,
  function(pkg, resource, field, ..., quiet = opt("quiet")) {
    covr::package_coverage(
      resource@path,
      type = "tests",
      quiet = quiet,
    )
  }
)

impl_data(
  "covr_test_coverage_fraction",
  metric = TRUE,
  class = class_double,
  permissions = c(),
  title = "Unit tests expression coverage",
  description = "the fraction of expressions evaluated by any unit tests",
  function(pkg, resource, field, ..., quiet = opt("quiet")) {
    covr::percent_coverage(pkg$covr_test_coverage) / 100
  }
)
