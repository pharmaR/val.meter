#' @eval options::as_roxygen_docs()
NULL

#' Options As Parameters
#' @eval options::as_params()
#' @name options_params
#' @keywords internal
NULL

#' @include utils_cli.R
define_options(
  fmt("Set the default `{packageName()}` policies, specifying how package
      resources will be discovered and what permissions are granted when
      calculating metrics."),
  policy = policy(),

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
  quiet = TRUE,

  fmt(
    "Character vector of `{{lintr}}` linter function names used when computing
    `lint_count` and `lint_free_fraction`. Each name must correspond
    to a linter constructor exported by `{{lintr}}` (e.g.
    `\"equals_na_linter\"`. Override to add, remove, or replace linters.
    Note: linters requiring configuration or environment-specific ones 
    (e.g. `\"backport_linter\"`) are excluded from the default set;
    add them manually by changing this option, possibly after configuring
    them globally."
  ),
  lint_linters = c(
    "absolute_path_linter",
    "all_equal_linter",
    "class_equals_linter",
    "download_file_linter",
    "equals_na_linter",
    "for_loop_index_linter",
    "length_test_linter",
    "list_comparison_linter",
    "nonportable_path_linter",
    "object_overwrite_linter",
    "package_hooks_linter",
    "pipe_return_linter",
    "redundant_equals_linter",
    "routine_registration_linter",
    "sample_int_linter",
    "seq_linter",
    "sprintf_linter",
    "strings_as_factors_linter",
    "T_and_F_symbol_linter",
    "terminal_close_linter",
    "unused_import_linter",
    "vector_logic_linter"
  ),

  fmt("Recognized source control hosting domains used when inferring whether a
      package has a source code repository on a recognized hosting platform.
      Customize this to add additional git hosting services (e.g., self-hosted
      GitLab instances or other federated git providers)."),
  source_control_domains = c(
    "github.com",
    "gitlab.com",
    "bitbucket.org",
    "r-forge.r-project.org",
    "codeberg.org",
    "sr.ht",              # sourcehut
    "gitea.com",
    "git.sr.ht"
  )
)
