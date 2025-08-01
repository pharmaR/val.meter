#' Tags Class
#'
#' A enumeration of tags that can be applied to a metric, to capture metadata
#' more effectively.
#' 
#' `r roxygenize_enum_vector(tags)`
#'
#' @keywords safeguard
#' @include utils_enum_vector.R
#' @export
tags <- class_tags <- enum_vector(
  name = "tags",
  parent = class_character,
  enum = c(
    "adoption",
    "lifecycle management",
    "best practice",
    "execution",
    "transient",
    "version-independent"
  )
)
