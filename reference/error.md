# Build an error from an error type and data attributes

Build an error from an error type and data attributes

## Usage

``` r
error(type, ...)
```

## Arguments

- type:

  `character(1L)` class type for error condition

- ...:

  Additional arguments passed to specific error function.

## Value

A error condition of class `type`

## Note

This function is only intended for use when parsing serialized output
from text.

This function uses an error class to build an error object. It
intentionally produces incomplete error objects, lacking the error
backtrace call.

For signalling errors internal to the package, see
[`err()`](https://pharmar.github.io/val.meter/reference/errors.md). This
function is used to provide a readable syntax to exported `PACKAGES`
files, which are parsed using this function back into their respective
error objects.

## Examples

``` r
# given a DCF input such as

## Package: testpkg
## Version: 1.2.3
## Metric/word_count@R: error("missing_suggests", "wordcount")

# we want to parse (by evaluation) the output into our own error type
error("missing_suggests", "wordcount")
#> <error/val_meter_missing_suggests_error>
#> ! data derivation requires suggests: wordcount
```
