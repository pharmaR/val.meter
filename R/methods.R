#' [`pkg()`] from `character`
#'
#' Attempt to find any suitable package sources.
#'
#' @noRd
method(pkg_from, class_character) <-
  function(x, ..., policy = opt("policy")) {
    all_resource_types <- append(policy@types, policy@accepted_source_types)

    # create an empty list to populate with discovered resources
    resources <- list()
    length(resources) <- length(all_resource_types)

    # helper function to add a resource to our discovered resource list
    add_resource <- function(resource) {
      idx <- match(list(S7_class(resource)), all_resource_types)
      if (is.na(idx) || !is.null(resources[[idx]])) return()
      resources[[idx]] <<- resource
    }

    # try to cast into each resource type
    for (resource_type in all_resource_types) {
      tryCatch(
        add_resource(convert(x, resource_type)),
        error = identity
      )
    }

    # filter out undiscovered resource types
    resources <- utils::head(resources, length(policy@types))
    resources <- Filter(Negate(is.null), resources)
    if (length(resources) < 1L) {
      stop("Unable to discover package resource")
    }

    # return pkg object with discovered resources
    new_pkg(resources = resources, ...)
  }



#' Derive by Field Name
#'
#' When a field is provided by name, create an empty S3 object using the field
#' name for further dispatch.
#'
#' @noRd
method(
  pkg_data_derive,
  list(class_character, class_any, new_union(class_any, class_missing))
) <-
  function(field, pkg, resource, ...) {
    pkg_data_derive(
      as_pkg_data(field),
      pkg,
      resource = NULL,
      ...,
      field_name = field
    )
  }

#' Derive using Pkg Resources
#'
#' If no explicit resource is provided, iterate through available package
#' resources and return the value using the first successful resource.
#'
#' @noRd
method(
  pkg_data_derive,
  list(class_any, new_pkg, new_union(NULL, class_missing))
) <-
  function(field, pkg, resource, ..., field_name) {
    for (r in pkg@resources) {
      result <- tryCatch(
        pkg_data_derive(field = field, pkg = pkg, resource = r, ...),
        error = identity
      )

      if (!inherits(result, "S7_error_method_not_found")) return(result)
    }

    err(
      class = "missing_derivation",
      "Package data '{field_name}' could not be derived from known resources",
    )
  }

method(convert, list(class_character, pkg_data_tags)) <-
  function(from, to) pkg_data_tags(from)

method(convert, list(NULL, pkg_data_tags)) <-
  function(from, to) convert(character(0L), to)

method(convert, list(class_character, pkg_data_permissions)) <-
  function(from, to) pkg_data_permissions(from)

method(convert, list(NULL, pkg_data_permissions)) <-
  function(from, to) convert(character(0L), to)

method(convert, list(class_character, pkg_install_resource)) <-
  function(from, to) {
    if (file.exists(from) && file.exists(file.path(from, "INDEX"))) {
      return(to(path = from))
    } else if (length(paths <- find.package(from, quiet = TRUE)) > 0L) {
      return(convert(paths[[1]], to))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_character, pkg_archive_source_resource)) <-
  function(from, to) {
    if (file.exists(from) && file.exists(file.path(from, "MD5"))) {
      return(to(path = from))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_character, pkg_source_resource)) <-
  function(from, to) {
    if (
      file.exists(from) &&
        !file.exists(file.path(from, "INDEX")) &&
        !file.exists(file.path(from, "MD5")) &&
        file.exists(file.path(from, "DESCRIPTION"))
    ) {
      return(to(path = from))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_character, pkg_repo_resource)) <-
  function(from, to) {
    ap <- available.packages()
    ap_idx <- Position(function(pkg) identical(from, pkg), ap[, "Package"])

    if (!is.na(ap_idx)) {
      return(to(
        package = ap[ap_idx, "Package"],
        version = ap[ap_idx, "Version"],
        repo = ap[ap_idx, "Repository"]
      ))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }
