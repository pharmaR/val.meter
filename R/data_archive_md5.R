#' @include impl_data.R

impl_data(
  "archive_md5",
  class = class_character,
  for_resource = source_archive_resource,
  function(pkg, resource, field, ...) {
    tools::md5sum(resource@path)
  }
)

impl_data(
  "archive_md5",
  function(pkg, resource, field, ...) {
    resource@md5
  }
)
