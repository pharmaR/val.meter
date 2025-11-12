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

  :   ns_tmp_root()

  option:

  :   val.meter.logs

  envvar:

  :   R_VAL_METER_LOGS (evaluated if possible, raw string otherwise)

- quiet:

  default:

  :   TRUE

  option:

  :   val.meter.quiet

  envvar:

  :   R_VAL_METER_QUIET (evaluated if possible, raw string otherwise)

## See also

options getOption Sys.setenv Sys.getenv
