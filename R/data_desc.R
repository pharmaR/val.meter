#' @include impl_data.R

#' @importFrom desc desc
impl_data(
  "desc",
  class = c("description", "R6"),
  for_resource = new_union(source_code_resource, install_resource),
  function(pkg, resource, ...) {
    desc::desc(resource@path)
  }
)

impl_data(
  "name",
  title = "Package name",
  class = class_character,
  function(pkg, resource, ...) {
    pkg$desc$get_field("Package")
  }
)

impl_data(
  "name",
  for_resource = repo_resource,
  function(pkg, resource, ...) {
    resource@package
  }
)

impl_data(
  "version",
  class = class_character,
  function(pkg, resource, ...) {
    pkg$desc$get_field("Version")
  }
)

impl_data(
  "version",
  for_resource = repo_resource,
  function(pkg, resource, ...) {
    resource@version
  }
)

impl_data(
  "dependency_count",
  class = class_integer,
  metric = TRUE,
  tags = c("best practice"),
  permissions = c(),
  title = "Dependency Count",
  description = "the number of required dependencies",
  function(pkg, resource, ...) {
    sum(pkg$desc$get_deps()$type %in% c("Depends", "Imports", "LinkingTo"))
  }
)

impl_data(
  "has_website",
  class = class_logical,
  metric = TRUE,
  permissions = c(),
  title = "Has Website",
  description = "a logical indicating whether the package has a website",
  function(pkg, resource, ...) {
    length(pkg$desc$get_urls()) > 0
  }
)

impl_data(
  "desc",
  for_resource = mock_resource,
  function(pkg, resource, ..., deps = NULL) {
    # mock a desc::desc object given a package cohort, generating data
    # dependencies
    desc <- desc::description$new("!new")
    desc$set("Package", resource@package)
    desc$set("Version", resource@version)
    desc$set("License", "Phony License")
    if (!is.null(deps)) {
      desc$set_deps(deps)
    }

    desc
  }
)
