# val.meter Options

Internally used, package-specific options. All options will prioritize R
options() values, and fall back to environment variables if undefined.
If neither the option nor the environment variable is set, a default
value is used.

## Checking Option Values

Option values specific to `val.meter` can be accessed by passing the
package name to `env`.

    options::opts(env = "val.meter")

    options::opt(x, default, env = "val.meter")

## Options

- policy:

  default:

  :   policy()

  option:

  :   val.meter.policy

  envvar:

  :   R_VAL_METER_POLICY (evaluated if possible, raw string otherwise)

- tags:

  default:

  :   tags(TRUE)

  option:

  :   val.meter.tags

  envvar:

  :   R_VAL_METER_TAGS (evaluated if possible, raw string otherwise)

- logs:

  default:

  :   FALSE

  option:

  :   val.meter.logs

  envvar:

  :   R_VAL_METER_LOGS (evaluated if possible, raw string otherwise)

- artifacts:

  default:

  :   ns_tmp_root()

  option:

  :   val.meter.artifacts

  envvar:

  :   R_VAL_METER_ARTIFACTS (evaluated if possible, raw string
      otherwise)

- quiet:

  default:

  :   TRUE

  option:

  :   val.meter.quiet

  envvar:

  :   R_VAL_METER_QUIET (evaluated if possible, raw string otherwise)

- lint_linters:

  default:

  :   c("absolute_path_linter", "all_equal_linter", "class_equals_linter",
              "download_file_linter", "equals_na_linter", "for_loop_index_linter",
              "length_test_linter", "list_comparison_linter", "nonportable_path_linter",
              "object_overwrite_linter", "package_hooks_linter", "pipe_return_linter",
              "redundant_equals_linter", "routine_registration_linter",
              "sample_int_linter", "seq_linter", "sprintf_linter", "strings_as_factors_linter",
              "T_and_F_symbol_linter", "terminal_close_linter", "unused_import_linter",
              "vector_logic_linter")

  option:

  :   val.meter.lint_linters

  envvar:

  :   R_VAL_METER_LINT_LINTERS (evaluated if possible, raw string
      otherwise)

- source_control_domains:

  default:

  :   c("github.com", "gitlab.com", "bitbucket.org", "r-forge.r-project.org",
              "codeberg.org", "sr.ht", "gitea.com", "git.sr.ht")

  option:

  :   val.meter.source_control_domains

  envvar:

  :   R_VAL_METER_SOURCE_CONTROL_DOMAINS (evaluated if possible, raw
      string otherwise)

## See also

options getOption Sys.setenv Sys.getenv
