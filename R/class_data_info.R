#' Various helpers for using a field name for method dispatch
#'
#' Given a field as a name, for example `"desc"`, we want to dispatch to the
#' right derivation method for this piece of data. To do this, we need to
#' convert this field name into an object that has a class that we can use
#' for method dispatch. These functions are helpers for standardizing this
#' conversion so that we don't need to concern ourselves with consistent
#' class names throughout this package.
#'
#' In most cases, to dispatch to a method for a field, one would
#'
#'     fn(as_pkg_data("field_name"), ...)
#'
#' Which creates an `S3` object with a corresponding class. For generics in this
#' package that might be dispatched by field name, they often have default
#' methods already implemented so that you can simplify this to:
#'
#'     fn("field_name", ...)
#'
#' And it will implicitly dispatch to the appropriate method.
#'
#' @keywords internal
#' @name pkg_data_dispatch
NULL

#' Discover implemented data fields
#'
#' This is used for finding all available metrics for use in [`metrics()`], as
#' well as for tab completions for `<pkg>$ <TAB>` to auto-populate a list of
#' available metrics.
#'
#' @param ... A list of [`S7`] classes. Not used if `args` is provided.
#' @param args A list of [`S7`] classes, by default, collects the elements of
#'   `...`.
#'
#' @returns A `character` vector of field names.
#'
#' @keywords internal
#' @name pkg_data_dispatch
get_data_derive_field_names <- function(..., args = list(...)) {
  if (length(args) == 0L) args <- list(class_pkg, class_resource)
  signature_prefix <- "pkg_data_field_"

  all_methods <- function(x) {
    if (is.function(x) && inherits(x, "S7_generic")) {
      all_methods(x@methods)
    } else if (is.environment(x)) {
      unlist(recursive = FALSE, use.names = FALSE, lapply(x, all_methods))
    } else {
      x
    }
  }

  # retrieve all methods
  methods <- all_methods(pkg_data_derive)
  signatures <- lapply(methods, attr, "signature")
  valid_signatures <- Filter(x = signatures, function(signature) {
    for (i in seq_along(args)) {
      if (!is_subclass(signature[[i]], args[[i]])) {
        return(FALSE)
      }
    }

    TRUE
  })

  # extract fields for which derivation is implemented given dispatch args
  signature_types <- unique(as.character(Filter(
    Negate(is.null),
    lapply(signatures, function(signature) {
      signature[[3]]$class[[1]]
    })
  )))

  # subset for only methods which dispatch on [`pkg_data()`] types
  is_pkg_data <- startsWith(signature_types, signature_prefix)
  signature_types <- signature_types[is_pkg_data]

  # remove prefix for completions, return only those that match pattern
  substring(signature_types, nchar(signature_prefix) + 1L)
}

#' Package Data and Metadata
#'
#' Package data is any information that can be derived about a package. Some of
#' these data are classified as "metrics" -- data that is structured, regular
#' and useful for systematic decision-making about packages. Metrics come with
#' additional constraints which ensure that data is calculated uniformly.
#'
#' @family data
#'
#' @include utils_s7.R
#' @include utils_rd.R
#' @include class_permissions.R
#' @include class_suggests.R
#' @include class_tags.R
data_info <- class_data_info <- new_class(
  "data_info",
  properties = list(
    #' @field metric `logical(1L)` a flag indicating whether the data is a
    #'   metric -- a piece of structured, regular package metadata useful for
    #'   decision-making about package use.
    metric = class_logical,

    #' @field title `character(1L)` a descriptive title.
    title = class_character,

    #' @field description `Rd` a short description about the data.
    #'   Should provide enough context to support effective decision-making.
    #'   Can use Rd formatting for rich text.
    description = new_property(
      S7::new_S3_class("Rd"),
      default = rd_empty(),
      setter = function(self, value) {
        if (!inherits(value, "Rd")) {
          if (is.character(value) && sum(nchar(value)) == 0L) {
            self@description <- rd_empty()
          } else {
            self@description <- tools::parse_Rd(
              textConnection(value),
              fragment = TRUE,
              permissive = TRUE
            )
          }
        } else {
          self@description <- value
        }

        self
      }
    ),

    #' @field tags [`val.meter::class_tags()`] a set of tags, used for
    #'   classifying metrics into categories.
    tags = new_property(
      class_tags,
      default = class_tags()
    ),

    #' @field data_class an object that can be converted into an `S7_class`
    #'   using [`S7::as_class()`], used for enforcing a return class on data
    #'   derivations.
    data_class = new_property(
      class_any,
      setter = function(self, value) {
        if (self@metric && !is_subclass(value, class_atomic)) {
          stop("metric data must have an atomic data class")
        }
        
        self@data_class <- value
        self
      },
      validator = function(value) {
        res <- tryCatch(S7::as_class(value), error = function(e) e)
        if (inherits(res, "error")) res$message
      }
    ),

    #' @field suggests `character(n)` a vector of suggested packages needed
    #'   for deriving a piece of data. If the package is not available, the
    #'   metric will not be derived.
    suggests = new_property(
      class_suggests,
      getter = function(self) {
        # expose internal data, allows subsetting disabled for S7 classes
        S7_data(self@suggests)
      },
      setter = function(self, value) {
        self@suggests <- value
        self
      }
    ),

    #' @field scopes [`permissions()`] a vector of enumerated permissions
    #'   that must be granted before this piece of data will be derived.
    scopes = class_permissions
  )
)

# NOTE:
#   required to avoid odd interaction when same external S3 generic is defined
#   for both S3 and S7 dispatch
#
#   https://github.com/RConsortium/S7/issues/390#issuecomment-2987133166
#
local({
  method(format, data_info) <-
    function(
      x,
      ...,
      permissions = opt("permissions"),
      tags = opt("tags")
    ) {
      class <- if (S7::S7_inherits(x@data_class)) { # nolint: object_usage_linter.
        x@data_class@name
      } else {
        S7:::class_desc(x@data_class)
      }

      is_installed <- vlapply(x@suggests, requireNamespace, quietly = TRUE)
      any_tags <- length(x@tags) + length(x@scopes) + length(x@suggests) > 0

      c(
        # title
        if (length(x@title) > 0L) {
          fmt(style_bold("{x@title} "))
        },
        
        
        # data type
        fmt(style_dim("{class}")),
        
        # description
        if (length(x@description) > 0L) {
          description <- paste0(
            collapse = " ",
            rd_to_txt(x@description, fragment = TRUE)
          )
          
          fmt(style_italic("\n{description}"))
        },
        
        if (any_tags) "\n",
        if (any_tags) {
          paste(collapse = " ", c(
            # tags
            vcapply(x@tags, function(tag) {
              color <- if (tag %in% tags) "blue" else "red"
              fmt(cli_tag(tag, scope = "", color = color))
            }),

            # permissions
            vcapply(x@scopes, function(scope) {
              color <- if (scope %in% permissions) "green" else "red"
              fmt(cli_tag(scope, scope = "req", color = color))
            }),

            # suggests dependencies
            vcapply(seq_along(x@suggests), function(i) {
              color <- if (is_installed[[i]]) "green" else "red"
              fmt(cli_tag(scope = "dep", x@suggests[[i]], color = color))
            })
          ))
        }
      )
    }
})

#' Retrieve package data field metadata, provided a character field name
#' @noRd
method(convert, list(class_character, data_info)) <-
  function(from, to) pkg_data_info(from)

# NOTE:
#   required to avoid odd interaction when same external S3 generic is defined
#   for both S3 and S7 dispatch
#
#   https://github.com/RConsortium/S7/issues/390#issuecomment-2987133166
#
local({
  method(print, data_info) <-
    function(
      x,
      permissions = opt("permissions"),
      tags = opt("tags"),
      ...
    ) {
      cat(paste(collapse = "", format(x, ...)), "\n")
      if (!all(x@scopes %in% permissions) || !all(x@tags %in% tags)) {
        cli_inform(
          paste0(
            "metric(s) will be disabled due to insufficient permissions or ",
            "restricted tags. See {.code ?{ packageName() }::options()} for ",
            "details about global policies."
          ),
          class = cnd_type("options", cnd = "message")
        )
      }

      invisible(x)
    }
})

#' @include utils_rd.R
method(toRd, data_info) <- function(x, ...) {
  has_tags <- length(x@tags) > 0
  requires_suggests <- length(x@suggests) > 0
  requires_permissions <- length(x@scopes) > 0
  
  class <- if (S7::S7_inherits(x@data_class)) {
    x@data_class@name
  } else {
    S7:::class_desc(x@data_class)
  }
  
  paste0(
    "\\code{", class, "} ",
    rd_deparse(x@description),
    "\n",
    toRd(x@scopes), " ",
    toRd(class_suggests(x@suggests)), " ",
    "\n\n",
    toRd(x@tags)
  )
}

#' @export
format.val_meter_error <- function(x, ...) {
  NextMethod(
    backtrace = FALSE,
    simplify = "branch",
    drop = TRUE,
    prefix = FALSE,
    ...
  )
}

#' @export
print.val_meter_error <- function(x, ...) {
  cat(format(x, ...), "\n")
}


#' A list of package data information
#'
#' This class is largely superficial. It's primary purpose is the
#' pretty-printing of package data metadata.
#'
#' @keywords internal
data_info_list <- new_class("data_info_list", parent = class_list)

#' Support pretty-printing by implementing `print` method
#' @noRd
local({
  method(print, data_info_list) <- function(x, ...) {
    msgs <- list()
  
    # use individual element print methods
    to_print <- x
    class(to_print) <- NULL
    attributes(to_print) <- NULL
    names(to_print) <- names(x)
  
    res <- withCallingHandlers(
      print(to_print),
      message = function(m) {
        if (inherits(m, cnd_type(cnd = "message"))) {
          msgs <<- append(msgs, list(m))
          invokeRestart("muffleMessage")
        }
      }
    )
  
    if (length(msgs) > 0L) {
      cli_alert_info("Some metrics will not be evaluated")
    }
  
    for (msg in unique(msgs)) {
      cli_text(style_dim(msg$message))
    }
  
    invisible(res)
  }
})

#' @include utils_rd.R
method(toRd, data_info_list) <- function(x, ...) {
  item_names <- vcapply(x, prop, "title")
  item_names <- ifelse(!is.na(item_names), item_names, names(x))
  pkg <- packageName()
  paste0(
    "\\section{Metrics}{",
    "The following metrics are provided by \\code{\\link{", pkg, "}}.",
    paste(collapse = "", "\n", vcapply(seq_along(x), function(i) {
      paste0("\\subsection{", item_names[[i]], "}{", toRd(x[[i]]), "}")
    })),
    "}"
  )
}

#' Package metric data
#'
#' @param x Optionally, an object to retrieve metrics from. When `NULL` (the
#'   default), a listing of metric metadata is returned.
#' @param ... Additional arguments unused.
#' @param all If `TRUE`, include non-metric package data. These are often
#'   intermediate data used in the calculation of package metrics.
#'
#' @returns A `list` of calculated values or metadata, in the cases where an
#'   object is or is not provided respectively.
#'
#' @evalRd tools::toRd(metrics())
#'
#' @export
metrics <- function(x, ..., all = FALSE) {
  fields <- get_data_derive_field_names()
  names(fields) <- fields
  fields <- lapply(fields, pkg_data_info)
  is_metric <- vlapply(fields, S7::prop, "metric")
  if (!all) fields <- fields[is_metric]
  data_info_list(fields)
}
