#' Create a new package object to start measuring package heuristics
#'
#' Aggregates package information, surveying metadata from discovered
#' resources.
#'
#' @keywords workflow
#' @include class_resource.R
#' @export
pkg <- class_pkg <- new_class(
  "pkg",
  properties = list(
    # data (not user-facing)
    # A mutable environment, used to aggregate package metadata. When working
    # with a [`pkg`] object, you may prefer to use the `[[` and `$` operators
    # for accessing data in this environment, which will also prompt any
    # necessary data dependencies to be evaluated.
    data = class_environment,

    #' @param resource [`resource`] (often a [`multi_resource`]), providing the
    #'   resources to be used for deriving packages data. If a
    #'   [`multi_resource`], the order of resources determines the precedence of
    #'   information. If information about a package could be derived from
    #'   multiple sources, the first source is prioritized.
    resource = S7::new_union(class_resource, class_mock_resource),

    #' @param permissions [`permissions`] granted for deriving data. If not
    #'   provided, the default from `policy` will be used.
    permissions = class_permissions

    #' @param policy [`policy`] to use when converting input to resources. Most
    #'   commonly used for interpreting strings as resources.
  ),

  constructor = function(
    resource,
    permissions,
    policy = opt("policy")
  ) {
    is_mocked <- S7::S7_inherits(resource, class_mock_resource)

    # handle anything that can be converted into a resource - especially
    # useful for character shorthands
    if (!is_mocked) {
      # TODO: warn when we won't use the policy; when a resource is provided
      resource <- convert(resource, class_resource, policy = policy)
    }

    if (!missing(permissions)) {
      permissions <- convert(permissions, class_permissions)
    } else {
      permissions <- policy@permissions
    }

    new_object(
      .parent = S7::S7_object(),
      data = new.env(parent = emptyenv()),
      metrics = list(),
      resource = resource,
      permissions = permissions
    )
  }
)

#' Generate Random Package(s)
#'
#' Create a package object to simulate metric derivation. When generating a
#' collection of packages, dependencies will realistically be made between
#' packages.
#'
#' @param package `character(1L)` a package name
#' @param version `character(1L)` a package version
#' @param md5 `character(1L)` an `md5` hash to use for the package. By default,
#'   is a simple hash derived from the package name and version.
#' @param ... Additional arguments passed to [`mock_resource`]
#' @param permissions `permissions` a permissions object, or a anything that
#'   can be interpretted as a permissions object through `convert`. Unlike
#'   general [`pkg`]s, [`random_pkg`]s default to mocking a package with all
#'   permissions are enabled to mock as many metrics as possible.
#'
#' @export
#' @name random_pkg
random_pkg <- function(
  package = random_pkg_name(),
  version = random_pkg_version(),
  md5 = md5sum(paste0(package, " v", version)),
  ...,
  permissions = TRUE
) {
  resource <- mock_resource(
    package = package,
    version = version,
    md5 = md5,
    ...
  )

  pkg(resource, permissions = permissions)
}

#' @describeIn random_pkg
#' Generate a set of random packges
#'
#' @param n `integer(1L)` how many packages to simulate
#'
#' @export
random_pkgs <- function(n = 100, ...) {
  pkg_names <- random_pkg_name(n = n)
  pkgs <- lapply(pkg_names, random_pkg, ...)

  pkg_deps <- if (requireNamespace("igraph", quietly = TRUE)) {
    pkg_graph <- random_pkg_graph(pkg_names)
    function(pkg_name, ...) random_pkg_igraph_deps(pkg_graph, pkg_name)
  } else {
    cli::cli_warn(
      "package cohort may have cyclic dependencies. install {.pkg igraph} to",
      "ensure acyclic relationship"
    )

    function(pkg_name) random_pkg_naive_deps(pkg_name, cohort = pkg_names)
  }

  # generate mock metrics
  for (i in seq_along(pkgs)) {
    # provide cohort of package names to desc for generating dependencies
    pkg_name <- pkg_names[[i]]
    pkg <- pkgs[[i]]
    get_pkg_data(pkg, "desc", deps = pkg_deps(pkg_name))
    metrics(pkg)
  }

  structure(pkgs, class = c("list_of_pkg", class(pkgs)))
}

#' @describeIn random_pkg
#' Create a random assortment of packages and write the out to local repository
#' file structure such that it can be used with `options(repos = random_repo())`
#'
#' @param path `character(1L)` directory path where the repository should be
#'   created. Directory will be created if it doesn't yet exist.
#'
#' @export
random_repo <- function(..., path = tempfile("repo")) {
  packages_path <- file.path(path, "src", "contrib", "PACKAGES")
  if (!dir.exists(d <- dirname(packages_path))) {
    dir.create(d, recursive = TRUE)
  }

  pkgs <- random_pkgs(...)
  dcf <- c(
    "Format: Metrics",
    "",
    to_dcf(pkgs)
  )
  writeLines(dcf, packages_path)

  repos <- paste0("file://", normalizePath(path))
  names(repos) <- paste0(packageName(), "-generated")

  repos
}

#' Get [`pkg`] object data
#'
#' This methods handles the error handling and propagation of deriving package
#' data. It is the primary interface by which a [`pkg`] object should be
#' deriving data. In contrast to [`pkg_data_derive`], which is the method that
#' individual data implements to register it as a field, this function wraps
#' the execution in appropriate error handling for user presentation. This
#' function should not throw errors, but instead should capture errors for
#' communication back to the user.
#'
#' @param x [`pkg`] object to derive data for
#' @param name `character(1L)` field name for the data to derive
#' @param ... Additional arguments unused
#' @param .raise `logical(1L)` flag indicating whether errors should be raised
#'   or captured. This flag is not intended to be set directly, it is exposed
#'   so that recursive calls can raise lower-level errors while capturing them
#'   at the surface.
#'
#' @returns the derived data, using the method of [`pkg_data_derive`] dispatched
#'   on field `name`.
#'
#' @keywords internal
#' @include utils_err.R
get_pkg_data <- function(x, name, ..., .raise = .state$raise) {
  # RStudio, when trying to produce completions,will try to evaluate our lazy
  # list elements. Intercept those calls and return only the existing values.
  if (is_rs_rpc_get_completions_call()) {
    return(as.list(x@data))
  }

  if (!exists(name, envir = x@data)) {
    # upon computing subsequent data dependencies, raise their errors so that
    # they can be captured and annotated as dependency errors
    if (!.raise) {
      .state$raise_derive_errors()
      on.exit(.state$raise_derive_errors(FALSE))
    }

    x@data[[name]] <- tryCatch(
      {
        info <- pkg_data_info(name)
        required_permissions <- info@permissions
        required_suggests <- info@suggests
        assert_permissions(required_permissions, x@permissions)
        assert_suggests(required_suggests)

        data <- pkg_data_derive(pkg = x, field = name, ...)
        if (!identical(info@data_class, class_any)) {
          data <- convert(data, info@data_class)
        }

        data
      },
      error = function(e, ...) {
        convert(e, class_val_meter_error, field = name)
      }
    )

    if (.raise && inherits(x@data[[name]], "error")) {
      stop(x@data[[name]])
    }
  }

  x@data[[name]]
}

get_pkg_datas <- function(x, index, ..., all = FALSE) {
  if (is.character(index)) {
    names(index) <- index
    return(lapply(index, get_pkg_data, x = x))
  }

  if (is.logical(index)) {
    if (length(index) != 1) {
      new_err("pkg objects can only be indexed with scalar logical values")
    }
    return(x[names(metrics(all = all))])
  }

  new_err("pkg objects don't know how to index with class {.cls index}")
}

#' @importFrom utils .DollarNames
#' @exportS3Method ".DollarNames" "val.meter::pkg"
`.DollarNames.val.meter::pkg` <- function(x, pattern, ...) {
  fields <- get_data_derive_field_names()
  fields[grepl(pattern, fields)]
}

#' @exportS3Method "$" "val.meter::pkg"
`$.val.meter::pkg` <- get_pkg_data

#' @exportS3Method "[[" "val.meter::pkg"
`[[.val.meter::pkg` <- get_pkg_data

#' @exportS3Method "[" "val.meter::pkg"
`[.val.meter::pkg` <- get_pkg_datas

#' @export
method(names, class_pkg) <- function(x, ...) {
  get_data_derive_field_names()
}

#' @export
method(print, class_pkg) <- function(x, ...) {
  class_header <- paste0("<", class(x)[[1]], ">")

  fields <- get_data_derive_field_names()
  names(fields) <- fields
  fields <- lapply(fields, convert, to = data_info)
  is_metric <- vlapply(fields, S7::prop, "metric")
  fields <- fields[order(!is_metric)]
  is_metric <- is_metric[order(!is_metric)]

  out <- paste0(
    class_header,
    "\n",
    "@resource",
    "\n",
    paste0("  ", capture.output(x@resource), collapse = "\n"),
    "\n",
    "@permissions",
    "\n",
    paste0("  ", capture.output(x@permissions), collapse = "\n"),
    "\n",
    paste0(
      collapse = "\n",
      "$",
      names(fields),
      ifelse(!is_metric, " (internal)", ""),
      vcapply(seq_along(fields), function(i) {
        field <- names(fields)[[i]]
        if (is_metric[[i]]) {
          if (exists(field, x@data)) {
            data <- x@data[[field]]
            if (inherits(data, cnd_type())) {
              output <- strsplit(format(data), "\n")[[1]]
              paste0("\n  ", output, collapse = "")
            } else {
              paste0("\n  ", capture.output(data), collapse = "")
            }
          } else {
            "\n  <promise>"
          }
        } else {
          ""
        }
      })
    )
  )

  cat(out, "\n")
}

#' @include utils_dcf.R
method(to_dcf, new_S3_class("list_of_pkg")) <- function(x, ...) {
  paste(collapse = "\n\n", vcapply(x, to_dcf))
}

#' @include utils_dcf.R
method(to_dcf, class_pkg) <- function(x, ...) {
  paste(
    collapse = "\n",
    c(
      to_dcf(x$desc),
      if (!is.na(x@resource@md5)) paste0("MD5sum: ", x@resource@md5),
      paste0("Metric/", names(metrics(x)), "@R: ", vcapply(metrics(x), to_dcf))
    )
  )
}

#' Produce `pkg`(s) from a `DCF`-formatted string
#'
#' @param x A `DCF`-formatted string
#' @param ... Additional arguments unused
#'
#' @returns A `pkg` object. Note that when parsing from a `DCF` string.
#'
#' @include utils_dcf.R
#' @export
#' @name pkg_from_dcf
pkg_from_dcf <- function(x, ...) {
  from_dcf(x, to = class_pkg, ...)
}

#' @include utils_dcf.R
#' @export
#' @name pkg_from_dcf
pkgs_from_dcf <- function(x, ...) {
  parts <- strsplit(x, "\n\n")[[1]]
  structure(lapply(parts, from_dcf, to = class_pkg), class = "list_of_pkg")
}

#' @exportS3Method as.data.frame list_of_pkg
as.data.frame.list_of_pkg <- function(x, ...) {
  # extract all package data types
  datas <- lapply(x, function(i) as.list(i@data))
  all_names <- unique(unlist(lapply(datas, names)))

  # build data frame
  df <- data.frame(
    package = vcapply(x, function(xi) xi@resource@package),
    version = vcapply(x, function(xi) xi@resource@version),
    md5 = vcapply(x, function(xi) xi@resource@md5)
  )

  # populate with data
  for (name in all_names) {
    df[[name]] <- simplify2array(lapply(x, function(xi) {
      data <- xi@data[[name]]
      is_err <- inherits(data, cnd_type())
      if (is_err) NA else data
    }))
  }

  df
}

#' @include utils_dcf.R
method(from_dcf, list(class_character, class_pkg)) <-
  function(x, to, ...) {
    dcf <- from_dcf(x, class_any)
    resource <- unknown_resource(
      package = dcf[[1, "Package"]],
      version = dcf[[1, "Version"]],
      md5 = if ("MD5sum" %in% colnames(dcf)) {
        dcf[[1, "MD5sum"]]
      } else {
        NA_character_
      }
    )

    data <- new.env(parent = emptyenv())
    prefix <- "Metric/"
    for (name in colnames(dcf)[startsWith(colnames(dcf), prefix)]) {
      field <- sub(prefix, "", name)
      info <- pkg_data_info(field)
      val <- dcf[[1, name]]
      val <- metric_coerce(val, info@data_class)
      data[[field]] <- val
    }

    pkg <- pkg(resource)
    pkg@data <- data

    pkg
  }
