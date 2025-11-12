# Tags Class

A enumeration of tags that can be applied to a metric, to capture
metadata more effectively.

## Usage

``` r
tags(...)
```

## Arguments

- ...:

  Values to include in enumerated vector. Multiple values will be
  concatenated into a vector before instantiation. Special handling if
  the first and only argument is a logical, interpreting `TRUE` as a
  vector of all enumerated values and `FALSE` as a zero-length vector.

## Accepted Values

- `"adoption"`

- `"lifecycle management"`

- `"best practice"`

- `"execution"`

- `"transient"`

- `"version-independent"`
