# Check whether we're currently evaluating RStudio's completions

Check whether we're currently evaluating RStudio's completions

## Usage

``` r
is_rs_rpc_get_completions_call(call = sys.call(1L))
```

## Arguments

- call:

  Which call to check. Defaults to the first call in the call stack,
  which will always be the originating call of RStudio's completions
  calls.
