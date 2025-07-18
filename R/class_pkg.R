#' Package Class
#'
#' Aggregates package information, surveying metadata from discovered
#' resources.
#'
#' @include class_resource.R
#' @export
pkg <- class_pkg <- new_class(
  "pkg",
  properties = list(
    #' @field data A mutable environment, used to aggregate package meatadata.
    #' When working with a [`pkg`] object, you may prefer to use the `[[` and
    #' `$` operators for accessing data in this environment, which will also
    #' prompt any necessary data dependencies to be evaluated.
    data = class_environment,

    #' @field metrics Return calculated listing of all metrics
    metrics = new_property(
      class_any,
      getter = function(self) self[TRUE]
    ),

    #' @field resource A [`resource`] (often a [`multi_resource`]), providing
    #' the resources to be used for deriving packages data. If a
    #' [`multi_resource`], the order of resources determines the precedence
    #' of information. If information about a package could be derived from
    #' multiple sources, the first source is prioritized.
    resource = S7::new_union(class_resource, class_mock_resource),

    #' @field scopes Permissible scopes for deriving data.
    scopes = permissions
  ),
  constructor = function(x, scopes = opt("permissions")) {
    is_mocked <- S7::S7_inherits(x, class_mock_resource)
    if (!is_mocked) x <- convert(x, resource)

    new_object(
      .parent = S7::S7_object(),
      data = new.env(parent = emptyenv()),
      metrics = list(),
      resource = x,
      scopes = scopes
    )
  }
)

#' @export
random_pkg <- function(
    package = random_pkg_name(),
    version = random_pkg_version(),
    ...) {
  resource <- mock_resource(
    package = package,
    version = version,
    md5 = tools::md5sum(bytes = charToRaw(paste0(package, " v", version)))
  )

  pkg(resource, ...)
}

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
    pkg@metrics
  }

  structure(pkgs, class = c("list_of_pkgs", class(pkgs)))
}

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
      pkg_data_derive(pkg = x, field = name, ...),
      error = identity
    )

    if (.raise && inherits(x@data[[name]], "error")) {
      stop(x@data[[name]])
    }
  }

  if (.raise && inherits(x@data[[name]], cnd_type())) {
    new_err(
      class = "derive_dependency",
      data = list(field = name),
      "field depends on field '{name}' that threw an error during derivation"
    )
  }

  x@data[[name]]
}

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
`[.val.meter::pkg` <- function(x, index, ..., all = FALSE) {
  if (is.character(index)) {
    names(index) <- index
    return(lapply(index, function(i) x[[i]]))
  }

  if (is.logical(index)) {
    if (length(index) != 1) {
      new_err("pkg objects can only be indexed with scalar logical values")
    }
    return(x[names(metrics(all = all))])
  }

  new_err("pkg objects don't know how to index with class {.cls index}")
}

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
    class_header, "\n",
    "@resource", "\n",
    paste0("  ", capture.output(x@resource), collapse = "\n"), "\n",
    "@scopes", "\n",
    paste0("  ", capture.output(x@scopes), collapse = "\n"), "\n",
    paste0(
      collapse = "\n",
      "$", names(fields),
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
method(to_dcf, new_S3_class("list_of_pkgs")) <- function(x, ...) {
  paste(collapse = "\n\n", vcapply(x, to_dcf))
}

#' @include utils_dcf.R
method(to_dcf, class_pkg) <- function(x, ...) {
  paste(collapse = "\n", c(
    to_dcf(x$desc),
    if (!is.na(x@resource@md5)) paste0("MD5: ", x@resource@md5),
    paste0("Metric/", names(x@metrics), "@R: ", vcapply(x@metrics, to_dcf))
  ))
}

#' @include utils_dcf.R
#' @export
pkg_from_dcf <- function(x, ...) {
  from_dcf(x, to = class_pkg, ...)
}

#' @include utils_dcf.R
#' @export
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
      md5 = if ("MD5" %in% colnames(dcf)) dcf[[1, "MD5"]] else NA_character_
    )

    data <- new.env(parent = emptyenv())
    prefix <- "Metric/"
    for (name in colnames(dcf)[startsWith(colnames(dcf), prefix)]) {
      field <- sub(prefix, "", name)
      info <- pkg_data_info(field)
      val <- dcf[[1, name]]

      # special case for integers without trailing "L"
      if (identical(info@data_class, class_integer) && is(val, class_numeric)) {
        val <- as.integer(val)
      }

      if (!inherits(val, cnd_type())) {
        val <- convert(val, info@data_class)
      }

      data[[field]] <- val
    }

    pkg <- pkg(resource)
    pkg@data <- data

    pkg
  }
