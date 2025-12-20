#' @eval options::as_roxygen_docs()
NULL

#' Options As Parameters
#' @eval options::as_params()
#' @name options_params
#' @keywords internal
NULL

#' @include utils_cli.R
define_options(
  fmt(
    "Set the default `{packageName()}` policies, specifying how package
      resources will be discovered and what permissions are granted when
      calculating metrics."
  ),
  policy = policy(),

  fmt(
    "Set the default `{packageName()}` tags policy. Tags characterize the
    types of information various metrics contain. For more details, see
    [`tags()`]."
  ),
  tags = tags(TRUE),

  fmt("Whether output should be captured during the evaluation of metrics."),
  logs = TRUE,

  fmt(
    "Directory where artifacts will be stored. This includes installation logs,
    package source code and temporary libraries used while evaluating packages."
  ),
  artifacts = ns_tmp_root(),

  "Silences console output during evaluation. This applies when pulling package
  resources (such as download and installation output) and executing code
  (for example, running `R CMD check`)",
  quiet = TRUE
)
