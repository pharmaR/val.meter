register_data(
  "desc",
  class = c("description", "R6"),
  pkg_local_resource = function(field, pkg, resource, ...) {
    desc::desc(resource@path)
  }
)

register_data(
  "name",
  class = class_character,
  function(field, pkg, resource, ...) {
    pkg$desc$get_field("Package")
  }
)

register_data(
  "version",
  class = class_character,
  function(field, pkg, resource, ...) {
    pkg$desc$get_field("Version")
  }
)
