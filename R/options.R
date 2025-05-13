#' @eval options::as_roxygen_docs()
NULL

#' Options As Parameters
#' @eval options::as_params()
#' @name options_params
NULL

define_options(
  fmt("Set the default {packageName()} package search policy."),
  policy = pkg_resource_policy(),

  fmt("
    Set the default {packageName()} data scopes policy. Scopes specify
    permissions permitted when deriving package data. For more details, see
    `?pkg_data_scopes`.
  "),

  scopes = pkg_data_scopes()
)
