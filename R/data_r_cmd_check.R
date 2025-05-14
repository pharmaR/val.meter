impl_data(
  "r_cmd_check",
  class = S7::new_S3_class("rcmdcheck"),
  tags = c("execution"),
  suggests = "rcmdcheck",
  permissions = "execution",
)

impl_data(
  "r_cmd_check", for_resource = pkg_source_resource,
  function(field, pkg, resource, ...) {
    rcmdcheck::rcmdcheck(resource@path)
  }
)

impl_data(
  "r_cmd_check_errors_count",
  metric = TRUE,
  class = class_integer,
  tags = c("execution"),
  permissions = c(),
  description = "the number of errors produced when running R CMD check",
  function(field, pkg, resource, ...) {
    length(pkg$r_cmd_check$errors)
  }
)
