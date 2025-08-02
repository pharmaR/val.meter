#' Package Data Permissions Class
#'
#' A descriptor of behaviors required for deriving package data. Each policy
#' property is a [`logical`] flag, which may be either `TRUE` or `FALSE`.
#' Policies are declared for each metric, as well as when deriving metrics for
#' a package. When deriving metrics, only those metrics which conform to the set
#' policy will be derived.
#'
#' Given a policy, only assessments which conform to all enabled flags will be
#' assessed.
#'
#' `r roxygenize_enum_vector(permissions)`
#'
#' @param ... Values to include in enumerated vector. Multiple values will be
#'   concatenated into a vector before instantiation. Special handling if the
#'   first and only argument is a logical, interpreting `TRUE` as a vector of
#'   all enumerated values and `FALSE` as a zero-length vector.
#'
#' @keywords safeguard
#' @include utils_enum_vector.R
#' @export
permissions <- class_permissions <- enum_vector(
  name = "permissions",
  parent = class_character,
  enum = c(
    "write",
    "execution",
    "network"
  )
)
