register_data(
  "r_cmd_check",
  class = S7::new_S3_class("rcmdcheck"),
  tags = c("execution"),
  suggests = "rcmdcheck",
  pkg_source_resource = function(field, pkg, resource, ...) {
    rcmdcheck::rcmdcheck(resource@path)
  }
)

register_metric(
  "r_cmd_check_errors_count",
  class = pkg_metric_integer,
  tags = c("execution"),
  scopes = c(),
  description = "the number of errors produced when running R CMD check",
  function(field, pkg, resource, ...) length(pkg$r_cmd_check$errors)
)
