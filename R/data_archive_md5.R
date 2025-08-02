#' @include impl_data.R

impl_data(
  "archive_md5",
  class = class_character,
  for_resource = source_archive_resource,
  function(pkg, resource, ...) {
    tools::md5sum(resource@path)
  }
)

impl_data(
  "archive_md5",
  function(pkg, resource, ...) {
    resource@md5
  }
)
