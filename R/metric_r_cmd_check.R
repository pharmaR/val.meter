register_data(
  "r_cmd_check",
  class = "rcmdcheck",
  tags = c("execution"),
  suggests = "rcmdcheck",
  scopes = "permit_execution",
  pkg_source_resource = function(field, pkg, resource, ...) {
    rcmdcheck::rcmdcheck(resource@path)
  }
)

register_metric(
  "r_cmd_check_errors_count",
  class = class_integer,
  tags = c("execution"),
  scopes = c(),
  description = "the number of errors produced when running R CMD check",
  function(field, pkg, resource, ...) length(pkg$r_cmd_check$errors)
)
