impl_data(
  "desc",
  class = c("description", "R6"),
  for_resource = pkg_local_resource,
  function(field, pkg, resource, ...) {
    desc::desc(resource@path)
  }
)

impl_data(
  "name",
  class = class_character,
  function(field, pkg, resource, ...) {
    pkg$desc$get_field("Package")
  }
)

impl_data(
  "version",
  class = class_character,
  function(field, pkg, resource, ...) {
    pkg$desc$get_field("Version")
  }
)
