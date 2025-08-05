#' Metric data frame
#'
#' A [`data.frame`], for all intents and purposes, with column names
#' corresponding to [`pkg`] metric data.
#'
#' @export
class_metric_data_frame <- new_class(
  "metric_data_frame",
  parent = class_data.frame
)

#' A matrix produced by `available.packages`
#'
#' @note
#' Due to a [limitation of `S7`'s handling of `matrix` and `array`
#' classes](https://github.com/RConsortium/S7/issues/401), we currently need
#' to wrap matrices that we want to handle through `convert` in our own custom
#' classes. This may not be necessary in the future.
#'
#' @export
class_package_matrix <- new_class(
  "package_matrix",
  parent = class_character
)

method(convert, list(class_package_matrix, class_metric_data_frame)) <-
  function(from, to, ..., errors = c("as.na", "as.is")) {
    errors <- match.arg(errors)
    metrics <- metrics()
    metric_names <- names(metrics)

    df <- as.data.frame(unclass(from))
    out <- df[c("Package", "Version", "MD5sum")]
    for (metric_name in metric_names) {
      metric <- metrics[[metric_name]]
      field_names <- paste0("Metric/", metric_name, c("", "@R"))
      if (!any(field_names %in% colnames(df))) next

      field_name <- field_names[which(field_names %in% colnames(df))[[1]]]
      out[[metric_name]] <- suppressWarnings({
        metric_coerce(df[[field_name]], metric@data_class)
      })

      if (identical(errors, "as.na")) next
      is_error <- is.na(out[[metric_name]])
      is_error <- grepl(
        paste0("^(", packageName(), "::)?error"),
        df[[field_name]][is_error]
      )

      out[[metric_name]][is_error] <- lapply(
        df[[field_name]][is_error],
        function(x) {
          eval(parse(text = x, keep.source = FALSE), envir = topenv())
        }
      )
    }

    out
  }
