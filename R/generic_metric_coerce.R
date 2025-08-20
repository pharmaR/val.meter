#' Metric Coerce
#'
#' Nearly identical to [`S7::convert`], but with a few additional helpers for
#' handling conversions between atomic types that are not implemented by
#' convert.
#'
#' @note
#' We avoid using [`convert`] directly to avoid _type piracy_ - defining
#' methods for signatures we don't control for generics we didn't create.
#' Since we created neither the `convert` generic, nor the base classes we
#' want to implement it for, we instead create our own generic to avoid
#' inadvertently affecting others' code.
#'
#' @inheritParams S7::convert
#'
#' @export
metric_coerce <- new_generic("metric_coerce", c("from", "to"))

method(metric_coerce, list(class_any, class_any)) <-
  function(from, to, ...) {
    to_dispatch <- c(class_desc(to), class_desc(class_any), class(to))
    to <- structure(to, class = to_dispatch)
    metric_coerce(from, to, ...)
  }

method(metric_coerce, list(class_any, new_S3_class(class_desc(class_any)))) <-
  convert

method(metric_coerce, list(new_S3_class(cnd_type()), class_any)) <-
  function(from, to, ...) {
    from
  }

method(
  metric_coerce,
  list(
    new_union(class_character, class_double, class_logical),
    new_S3_class(class_desc(class_integer))
  )
) <-
  function(from, to, ...) {
    as.integer(from, ...)
  }
