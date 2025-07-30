#' @eval options::as_roxygen_docs()
NULL

#' Options As Parameters
#' @eval options::as_params()
#' @name options_params
#' @keywords internal
NULL

#' @include utils_cli.R
define_options(
  fmt("Set the default `{packageName()}` package search policy."),
  policy = resource_policy(),

  fmt("Set the default `{packageName()}` data permissions policy. Permissions
    specify what capabilities are permitted when deriving package data. For more
    details, see [`permissions()`]."),
  permissions = permissions(FALSE),

  fmt("Set the default `{packageName()}` tags policy. Tags characterize the
    types of information various metrics contain. For more details, see
    [`tags()`]."),
  tags = tags(TRUE),

  fmt("Logging directory where artifacts will be stored. Defaults to a temporary
    directory."),
  logs = ns_tmp_root(),

  "Silences console output during evaluation. This applies when pulling package
  resources (such as download and installation output) and executing code
  (for example, running `R CMD check`)",
  quiet = TRUE
)
