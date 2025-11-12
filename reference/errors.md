# Package data derivation error handling

Because we aim to capture errors that are raised when evaluating
packages, there is a fair bit of machinery to raise relevant errors when
package assertions fail and capture errors during data execution.

## Usage

``` r
.state

once_on_task_callback(name, expr, envir = parent.frame())

get_package_boundary_call(calls = sys.calls())

cnd_type(class = NULL, cnd = "error")

cnd_class_from_type(type, cnd = "error")

new_err(
  ...,
  data = list(),
  class = NULL,
  call = NULL,
  trace = NULL,
  parent = NULL,
  capture = FALSE,
  .envir = parent.frame()
)

err
```

## Format

An object of class `environment` of length 2.

An object of class `list` of length 6.

## Functions

- `.state`: Global error raising flags

  This flag is used to determine when errors during execution should be
  captured or thrown to the evaluating environment. When data is being
  derived for the first time, we want to capture any errors, but when
  errors arrive during other uses of that data, we want to raise them to
  the user.

- `once_on_task_callback()`: Used to reset global state after a top
  level callback completes

- `get_package_boundary_call()`: Walk the call stack to find the last
  call before the package boundary. This allows us to raise the most
  relevant parts of error messages back to users without exposing them
  to the internal non-standard evaluation calls.

- `cnd_type()`: Create a condition type

- `cnd_class_from_type()`: Extract a condition class from a type

- `new_err()`: Create a new error

  This function is a wrapper around
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
  but with defaults that make typical use within this package more
  interpretable to end-users.

  Used predominately by `err()`

- `err`: Raise a new error, using one of a set of known error types

## Fields

- `disallowed_permissions`:

  Create an error indicating that a data derivation requires permissions
  that were not permitted at execution time.

- `missing_suggests`:

  Create an error indicating that a dependency that is required for a
  specific data derivation is not available.

- `metric_not_atomic`:

  Create an error indicating that a metric was derived but did not
  conform to its anticipated atomic return type.

- `derive_dependency`:

  Create an error that is raised when a dependent data field threw an
  error during execution.

- `data_not_implemented`:

  Create an error indicating that data could not be derived because it
  is not implemented for this resource.

- `derive_error`:

  Wrap an error raised through data derivation in a packag error type
  for communication.
