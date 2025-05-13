get_data_derive_field_names <- function() {
  signature_prefix <- "pkg_data_field_"

  # get dispatch class types for first argument (field name)
  signature_types <- names(pkg_data_derive@methods)

  # subset for only methods which dispatch on [`pkg_data()`] types
  is_pkg_data <- startsWith(signature_types, signature_prefix)
  signature_types <- signature_types[is_pkg_data]

  # remove prefix for completions, return only those that match pattern
  substring(signature_types, nchar(signature_prefix) + 1L)
}

metric <- new_class(
  "metric",
  properties = list(
    metric = class_logical,
    description = class_character,
    tags = pkg_data_tags,
    type = new_property(class_any, validator = function(value) {
      res <- tryCatch(S7::as_class(value), error = function(e) e)
      if (inherits(res, "error")) res$message
    }),
    suggests = class_character,
    scopes = pkg_data_scopes
  )
)

method(print, metric) <-
  function(x, ...) {
    fields <- S7::prop_names(x)
    n <- max(nchar(fields))

    type <- NULL
    if (S7::S7_inherits(x@type)) {
      type <- x@type@name
    } else {
      type <- S7:::class_desc(x@type)
    }

    lines <- Filter(Negate(is.null), list(
      cli::style_dim("{type}"),
      if (nchar(x@description) > 0L)
        cli::style_italic("{x@description}"),
      if (length(x@tags) > 0L) {
        paste("tags: ", paste(collapse = " ", vcapply(x@tags, function(tag) {
          cli::col_blue(cli::format_inline("[{tag}]"))
        })))
      },
      if (length(x@scopes)) {
        scopes <- paste(collapse = " ", vcapply(x@scopes, function(scopes) {
          cli::col_blue(cli::format_inline("[{scopes}]"))
        }))
        paste("requires permissions:", scopes)
      },
      if (length(x@suggests)) {
        is_installed <- vlapply(x@suggests, requireNamespace, quietly = TRUE)
        suggests <- vcapply(seq_along(x@suggests), function(i) {
          if (is_installed[[i]]) {
            cli::col_green(cli::format_inline("{cli::symbol$tick} {x@suggests[[i]]}"))
          } else {
            cli::col_red(cli::format_inline("{cli::symbol$cross} {x@suggests[[i]]}"))
          }
        })

        paste("requires packages:", suggests)
      }
    ))

    fmt <- vcapply(lines, cli::format_inline, .envir = environment())
    fmt <- paste(fmt, collapse = "\n")
    cat(fmt, "\n")
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

method(convert, list(class_character, metric)) <-
  function(from, to) {
    metric(
      metric = pkg_data_is_metric(from),
      description = pkg_data_get_description(from),
      tags = pkg_data_get_tags(from),
      type = pkg_data_get_class(from),
      suggests = pkg_data_get_suggests(from),
      scopes = pkg_data_get_scopes(from)
    )
  }

#' @export
metrics <- function(all = FALSE) {
  fields <- get_data_derive_field_names()
  names(fields) <- fields
  fields <- lapply(fields, convert, to = metric)
  is_metric <- vlapply(fields, S7::prop, "metric")
  if (!all) fields <- fields[is_metric]
  fields
}
