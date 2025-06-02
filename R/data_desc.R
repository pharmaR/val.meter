#' @include trait_pkg_data.R

impl_data(
  "desc",
  class = c("description", "R6"),
  for_resource = local_resource,
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

impl_data(
  "dependency_count",
  class = class_integer,
  metric = TRUE,
  tags = c("best practice"),
  permissions = c(),
  description = "the number of required dependencies",
  function(field, pkg, resource, ...) {
    sum(pkg$desc$get_deps()$type %in% c("Depends", "Imports", "LinkingTo"))
  }
)
