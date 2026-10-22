# Options As Parameters

Options As Parameters

## Arguments

- lint_linters:

  Character vector of `{lintr}` linter function names used when
  computing `lint_count` and `lint_free_fraction`. Each name must
  correspond to a linter constructor exported by `{lintr}` (e.g.
  `"equals_na_linter"`. Override to add, remove, or replace linters.
  Note: linters requiring configuration or environment-specific ones
  (e.g. `"backport_linter"`) are excluded from the default set; add them
  manually by changing this option, possibly after configuring them
  globally. (Defaults to
  `c("absolute_path_linter", "all_equal_linter", "class_equals_linter", ; "download_file_linter", "equals_na_linter", "for_loop_index_linter", ; "length_test_linter", "list_comparison_linter", "nonportable_path_linter", ; "object_overwrite_linter", "package_hooks_linter", "pipe_return_linter", ; "redundant_equals_linter", "routine_registration_linter", ; "sample_int_linter", "seq_linter", "sprintf_linter", "strings_as_factors_linter", ; "T_and_F_symbol_linter", "terminal_close_linter", "unused_import_linter", ; "vector_logic_linter")`,
  overwritable using option 'val.meter.lint_linters' or environment
  variable 'R_VAL_METER_LINT_LINTERS')

- tags:

  Set the default `val.meter` tags policy. Tags characterize the types
  of information various metrics contain. For more details, see
  [`tags()`](https://pharmar.github.io/val.meter/reference/tags.md).
  (Defaults to `tags(TRUE)`, overwritable using option 'val.meter.tags'
  or environment variable 'R_VAL_METER_TAGS')

- artifacts:

  Directory where artifacts will be stored. This includes installation
  logs, package source code and temporary libraries used while
  evaluating packages. (Defaults to `ns_tmp_root()`, overwritable using
  option 'val.meter.artifacts' or environment variable
  'R_VAL_METER_ARTIFACTS')

- quiet:

  Silences console output during evaluation. This applies when pulling
  package resources (such as download and installation output) and
  executing code (for example, running `R CMD check`) (Defaults to
  `TRUE`, overwritable using option 'val.meter.quiet' or environment
  variable 'R_VAL_METER_QUIET')

- policy:

  Set the default `val.meter` policies, specifying how package resources
  will be discovered and what permissions are granted when calculating
  metrics. (Defaults to
  [`policy()`](https://pharmar.github.io/val.meter/reference/policy.md),
  overwritable using option 'val.meter.policy' or environment variable
  'R_VAL_METER_POLICY')

- logs:

  Whether output should be captured during the evaluation of metrics.
  (Defaults to `FALSE`, overwritable using option 'val.meter.logs' or
  environment variable 'R_VAL_METER_LOGS')

- source_control_domains:

  Recognized source control hosting domains used when inferring whether
  a package has a source code repository on a recognized hosting
  platform. Customize this to add additional git hosting services (e.g.,
  self-hosted GitLab instances or other federated git providers).
  (Defaults to
  `c("github.com", "gitlab.com", "bitbucket.org", "r-forge.r-project.org", ; "codeberg.org", "sr.ht", "gitea.com", "git.sr.ht")`,
  overwritable using option 'val.meter.source_control_domains' or
  environment variable 'R_VAL_METER_SOURCE_CONTROL_DOMAINS')
