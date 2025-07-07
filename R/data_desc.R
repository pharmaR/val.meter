#' @include trait_pkg_data.R

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
  class = class_character,
  function(pkg, resource, ...) {
    pkg$desc$get_field("Package")
  }
)

impl_data(
  "name",
  class = class_character,
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
  class = class_character,
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
  description = "the number of required dependencies",
  function(pkg, resource, ...) {
    sum(pkg$desc$get_deps()$type %in% c("Depends", "Imports", "LinkingTo"))
  }
)

impl_data(
  "desc",
  for_resource = mock_resource,
  function(pkg, resource, ..., cohort = random_pkg_name(n = 100)) {
    # mock a desc::desc object given a package cohort, generating data
    # dependencies

    desc <- desc::description$new("!new")
    desc$set("Package", resource@package)
    desc$set("Version", resource@version)
    desc$set("License", "Phony License")

    # NOTE: naive dependency simulation, to do this robustly we would need
    #   to ensure that we aren't creating cyclic dependencies
    deps <- list()
    cohort <- setdiff(cohort, resource@package)
    deps$Depends <- c("R", sample(cohort, min(rpois(1, 0.5), length(cohort))))
    cohort <- setdiff(cohort, deps$Depends)
    deps$LinkingTo <- sample(cohort, min(rpois(1, 0.5), length(cohort)))
    cohort <- setdiff(cohort, deps$LinkingTo)
    deps$Imports <- sample(cohort, min(rpois(1, 2), length(cohort)))
    cohort <- setdiff(cohort, deps$Imports)
    deps$Suggests <- sample(cohort, min(rpois(1, 2), length(cohort)))

    desc$set_deps(data.frame(
      type = rep(names(deps), times = viapply(deps, length)),
      package = unlist(deps, use.names = FALSE),
      version = "*"
    ))

    desc
  }
)
