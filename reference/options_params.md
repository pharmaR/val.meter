# Options As Parameters

Options As Parameters

## Arguments

- tags:

  Set the default `val.meter` tags policy. Tags characterize the types
  of information various metrics contain. For more details, see
  [`tags()`](https://pharmar.github.io/val.meter/reference/tags.md).
  (Defaults to `tags(TRUE)`, overwritable using option 'val.meter.tags'
  or environment variable 'R_VAL_METER_TAGS')

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

  Logging directory where artifacts will be stored. Defaults to a
  temporary directory. (Defaults to `ns_tmp_root()`, overwritable using
  option 'val.meter.logs' or environment variable 'R_VAL_METER_LOGS')

- source_control_domains:

  Recognized source control hosting domains used when inferring whether
  a package has a source code repository on a recognized hosting
  platform. Customize this to add additional git hosting services (e.g.,
  self-hosted GitLab instances or other federated git providers).
  (Defaults to
  `c("github.com", "gitlab.com", "bitbucket.org", "r-forge.r-project.org", ; "codeberg.org", "sr.ht", "gitea.com", "git.sr.ht")`,
  overwritable using option 'val.meter.source_control_domains' or
  environment variable 'R_VAL_METER_SOURCE_CONTROL_DOMAINS')
