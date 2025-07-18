#' Resource Class
#'
#' A package resource is a resource for producing package information.
#' Package resources can vary from source code repositories to R package
#' repository listings. Each resources should be able to produce a downloadable
#' version of executable package code, but the extent of included code might
#' vary depending on source.
#'
#' Package resources should also implement `convert()`, providing a method for
#' converting into a more desirable source of information. For example, given
#' only a listing of a package in a repository, calling `convert()` on such
#' a reference may populate a local directory with the source code of the
#' package.
#'
#' @export
#' @name resource
resource <- class_resource <- new_class(
  "resource",
  abstract = TRUE,
  properties = list(
    #' @field optional id used for tracking resources throughout execution.
    #'   For example, the package source code from a [`repo_resource()`] may be
    #'   downloaded to add a [`archive_source_resource()`] and add it to a new
    #'   [`multi_resource()`]. Because all of these represent the same package,
    #'   they retain the same `id`. Primarily the `id` is used for
    #'   isolating temporary files.
    id = new_property(class_integer, default = quote(next_id())),
    md5 = new_property(class_character, default = NA_character_),
    package = new_property(class_character, default = NA_character_),
    version = new_property(class_character, default = NA_character_)
  )
)

mock_resource <- class_mock_resource <- new_class(
  "mock_resource",
  parent = resource
)

unknown_resource <- class_unknown_resource <- new_class(
  "unknown_resource",
  parent = resource
)

multi_resource <- class_multi_resource <- new_class(
  "multi_resource",
  parent = resource,
  properties = list(
    resources = new_property(
      class_list,
      validator = function(value) {
        if (!all(vlapply(value, S7_inherits, resource))) {
          "can only be constructed from a list of resources"
        }
      }
    )
  )
)

#' Package Source Code Resource Class
#'
#' Note that in some cases, this is distinct from the source code extracted
#' from a package archive, as some files are sometimes excluded from package
#' builds (uploaded to repositories). This can include additional development-
#' related metadata files, but can sometimes include additional tests that take
#' too long to run by automated systems by the repository host.
#'
#' Commonly, this may mean omitting tests to avoid failures on CRAN's automated
#' build systems.
#'
#' @export
local_resource <- class_local_resource <- new_class(
  "local_resource",
  parent = resource,
  abstract = TRUE,
  properties = list(
    path = new_property(
      class_character,
      default = NA_character_,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || !file.exists(value)) {
          "invalid path"
        }
      }
    )
  )
)

#' @export
remote_source_resource <- class_remote_source_resource <- new_class(
  "remote_source_resource",
  abstract = TRUE,
  parent = resource
)

#' @export
local_source_resource <- class_local_source_resource <- new_class(
  "local_source_resource",
  abstract = TRUE,
  parent = local_resource
)

#' Package Archive Source Code Resource Class
#'
#' The extracted source code from a package's build archive.
#'
#' @export
source_archive_resource <- class_source_archive_resource <- new_class(
  "source_archive_resource",
  parent = local_resource,
)

#' Package Install Resource Class
#'
#' An installed version of a package, as would be found in a package library.
#'
#' @export
install_resource <- class_install_resource <- new_class(
  "install_resource",
  parent = local_resource
)

#' Package Local Resource Class Union
#'
#' A union of all package resource classes that have local source code.
#'
#' @export
source_code_resource <- class_source_code_resource <- new_class(
  "source_code_resource",
  parent = local_source_resource
)

#' Package Repository Resource Class
#'
#' A reference to a listing in an R package repository.
#'
#' @export
repo_resource <- class_repo_resource <- new_class(
  "repo_resource",
  parent = resource,
  properties = list(
    repo = class_character
  )
)

#' Package `git` Resource Class
#'
#' A reference to a listing in an R package git source code repository.
#'
#' @export
git_resource <- class_git_resource <- new_class(
  "git_resource",
  parent = remote_source_resource,
  properties = list(
    http_url = class_character
  )
)

#' [`resource`] from `character`
#'
#' Attempt to find any suitable package sources.
#'
#' @noRd
method(convert, list(class_character, class_resource)) <-
  function(from, to, ..., policy = opt("policy")) {
    all_resource_types <- append(
      policy@accepted_resources,
      policy@source_resources
    )

    all_resource_type_names <- vcapply(all_resource_types, class_desc)

    # create an empty list to populate with discovered resources
    resources <- list()
    length(resources) <- length(all_resource_types)

    # helper function to add a resource to our discovered resource list
    add_resource <- function(resource) {
      resource_type_name <- class_desc(S7::S7_class(resource))
      idx <- match(resource_type_name, all_resource_type_names)
      if (is.na(idx) || !is.null(resources[[idx]])) {
        return()
      }
      resources[[idx]] <<- resource
      idx
    }

    # try to cast input into each resource type
    for (resource_type in all_resource_types) {
      new_idx <- tryCatch(
        add_resource(convert(from, resource_type)),
        error = identity
      )

      # stop on the first discovered source
      if (!inherits(new_idx, "error")) {
        break
      }
    }

    # after we've found all the resources we can parse a string into, try to
    # cast to any other resource types (eg, downloading source from repo)
    from_idx <- 1L
    while (from_idx <= length(resources)) {
      # for each resource
      from_resource <- resources[[from_idx]]
      if (!is.null(from_resource)) {
        # iterate over other allowed resource types
        for (to_idx in seq_along(all_resource_types)) {
          # that are not yet populated with a known resource
          to_resource <- resources[[to_idx]]
          if (!is.null(to_resource)) {
            next
          }

          # and try to convert a known resource to another desired resource type
          to_resource_type <- all_resource_types[[to_idx]]
          new_idx <- tryCatch(
            add_resource(convert(from_resource, to_resource_type)),
            error = identity
          )

          if (inherits(new_idx, "error")) {
            next
          }

          # if we found a more preferential resource, start again from that one
          if (is.integer(new_idx) && new_idx < from_idx) {
            from_idx <- new_idx - 1L # -1 to compensate for increment below
            break # from inner for-loop, continue with next while
          }
        }
      }

      from_idx <- from_idx + 1L
    }

    # filter out undiscovered resource types
    resources <- utils::head(resources, length(policy@accepted_resources))
    resources <- Filter(Negate(is.null), resources)

    if (length(resources) < 1L) {
      stop("Unable to discover package resource")
    }

    md5s <- vcapply(resources, S7::prop, "md5")
    if (length(resources) == 1L) {
      resources[[1L]]
    } else {
      if (length(unique(md5s)) != 1L) {
        warning("Discovered resources for \"{from}\" vary in their md5 hashes.")
      }

      multi_resource(
        package = resources[[1]]@package,
        version = resources[[1]]@version,
        md5 = resources[[1]]@md5,
        resources = resources
      )
    }
  }

method(convert, list(class_resource, class_resource)) <-
  function(from, to) {
    if (S7::S7_inherits(from, class = to)) {
      return(from)
    }
    from_str <- S7:::class_desc(from_class) # nolint
    to_str <- S7:::class_desc(to) # nolint
    stop(fmt("{from_str} cannot be cast into a {to_str}"))
  }

# mask built-in convert up/down-casting conversions
method(convert, list(class_resource, class_any)) <-
  function(from, to) {
    from_class <- attr(from, "S7_class")
    from_str <- S7:::class_desc(from_class) # nolint
    to_str <- S7:::class_desc(to) # nolint
    stop(fmt("Unable to convert from {from_str} to {to_str}"))
  }

method(convert, list(class_character, class_install_resource)) <-
  function(from, to) {
    if (file.exists(from) && file.exists(file.path(from, "INDEX"))) {
      return(to(path = normalizePath(from)))
    } else if (length(paths <- find.package(from, quiet = TRUE)) > 0L) {
      return(convert(paths[[1]], to))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_character, class_source_archive_resource)) <-
  function(from, to) {
    if (file.exists(from) && endsWith(from, ".tar.gz")) {
      return(to(path = normalizePath(from)))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_character, class_source_code_resource)) <-
  function(from, to) {
    if (
      file.exists(from) &&
        !file.exists(file.path(from, "INDEX")) &&
        file.exists(file.path(from, "DESCRIPTION"))
    ) {
      return(to(path = normalizePath(from)))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_character, class_repo_resource)) <-
  function(from, to) {
    ap <- available.packages()
    ap_idx <- Position(function(pkg) identical(from, pkg), ap[, "Package"])

    if (!is.na(ap_idx)) {
      return(to(
        package = ap[[ap_idx, "Package"]],
        version = ap[[ap_idx, "Version"]],
        md5 = ap[[ap_idx, "MD5sum"]],
        repo = ap[[ap_idx, "Repository"]]
      ))
    }

    stop(fmt("Cannot convert string '{from}' into {.cls to}"))
  }

method(convert, list(class_repo_resource, class_install_resource)) <-
  function(from, to, ..., policy = opt("policy"), quiet = opt("quiet")) {
    assert_scopes(c("network", "write"), policy@scopes)

    # before creating install resource, get a new resource id for install path
    id <- next_id()
    lib_path <- resource_dir(.id = id, "lib")

    install.packages(
      pkgs = from@package,
      lib = lib_path,
      contriburl = from@repo,
      repos = NULL,
      quiet = quiet
    )

    pkg_dir <- list.files(lib_path, full.names = TRUE)
    stopifnot(length(pkg_dir) == 1L)

    pkg_desc <- read.dcf(file.path(pkg_dir, "DESCRIPTION"))
    package <- pkg_desc[[1L, "Package"]]
    version <- pkg_desc[[1L, "Version"]]

    install_resource(
      id = id,
      path = pkg_dir,
      package = package,
      md5 = from@md5,
      version = version
    )
  }

method(convert, list(class_repo_resource, class_source_archive_resource)) <-
  function(from, to, ..., policy = opt("policy"), quiet = opt("quiet")) {
    assert_scopes("network", policy@scopes)

    # before creating the resource, get resource id to specify download path
    id <- next_id()
    path <- resource_dir(.id = id, "archive")

    x <- download.packages(
      pkgs = from@package,
      destdir = path,
      repos = NULL,
      contriburl = from@repo,
      quiet = quiet
    )

    package <- x[[1, 1]]
    path <- x[[1, 2]]
    version <- gsub("^.*/[^_]*_(.*)\\.tar\\.gz$", "\\1", path)

    source_archive_resource(
      id = id, # once downloaded, reuse id so we can avoid re-download
      package = package,
      version = version,
      md5 = from@md5,
      path = path
    )
  }

method(convert, list(class_local_source_resource, class_install_resource)) <-
  function(from, to, ..., policy = opt("policy"), quiet = opt("quiet")) {
    assert_scopes("write", policy@scopes)

    # before creating install resource, get a new resource id for install path
    id <- next_id()
    lib_path <- resource_dir(.id = id, "lib")

    install.packages(
      pkgs = from@path,
      lib = lib_path,
      repos = NULL,
      type = "source",
      quiet = quiet
    )

    pkg_dir <- list.files(lib_path, full.names = TRUE)
    stopifnot(length(pkg_dir) == 1L)

    pkg_desc <- read.dcf(file.path(pkg_dir, "DESCRIPTION"))
    package <- pkg_desc[[1L, "Package"]]
    version <- pkg_desc[[1L, "Version"]]

    install_resource(
      id = id,
      path = pkg_dir,
      package = package,
      md5 = from@md5,
      version = version
    )
  }

method(convert, list(class_resource, class_unknown_resource)) <-
  function(from, to) {
    set_props(to(), props(from, names(to@properties)))
  }

method(to_dcf, class_resource) <- function(x, ...) {
  new_err("Resource {.cls x} cannot be encoded for output to a DCF file.")
}

method(
  to_dcf,
  S7::new_union(class_local_source_resource, class_install_resource)
) <-
  function(x, ...) {
    deps <- c("Depends", "Imports", "LinkingTo", "Suggests")
    fields <- c("Package", "Version", deps, "License", "NeedsCompilation")
    desc <- read.dcf(file.path(x@path, "DESCRIPTION"), fields = fields)
    out <- character(0L)
    con <- textConnection("out", open = "w", local = TRUE)
    write.dcf(desc, con, keep.white = FALSE)
    out
  }

method(to_dcf, class_multi_resource) <- function(x, ...) {
  for (resource in x@resources) {
    result <- tryCatch(
      to_dcf(resource, ...),
      error = identity
    )

    if (!inherits(result, "error")) {
      return(result)
    }
  }

  cls <- class(x)[[1]] # nolint
  new_err("Resource {.cls {cls}} has no resources which produce a DCF output.")
}

method(to_dcf, new_S3_class("description")) <- function(x, ...) {
  deps <- c("Depends", "Imports", "LinkingTo", "Suggests")
  fields <- c("Package", "Version", deps, "License", "NeedsCompilation")
  fields <- x$get(fields)
  fields <- Filter(Negate(is.na), fields)
  paste0(
    names(fields), ": ",
    trimws(gsub("\\s+", " ", fields)),
    collapse = "\n"
  )
}

method(to_dcf, class_mock_resource) <- function(x, ...) {
  paste0(
    "# mocked data generated by ", packageName(), "\n",
    to_dcf(x@desc, ...)
  )
}
