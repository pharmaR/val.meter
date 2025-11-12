# Metric Coerce

Nearly identical to
[`S7::convert()`](https://rconsortium.github.io/S7/reference/convert.html),
but with a few additional helpers for handling conversions between
atomic types that are not implemented by convert.

## Usage

``` r
metric_coerce(from, to, ...)
```

## Arguments

- from:

  An S7 object to convert.

- to:

  An S7 class specification, passed to
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.html).

- ...:

  Other arguments passed to custom
  [`convert()`](https://rconsortium.github.io/S7/reference/convert.html)
  methods. For upcasting, these can be used to override existing
  properties or set new ones.

## Note

We avoid using
[`S7::convert()`](https://rconsortium.github.io/S7/reference/convert.html)
directly to avoid *type piracy* - defining methods for signatures we
don't control for generics we didn't create. Since we created neither
the `convert` generic, nor the base classes we want to implement it for,
we instead create our own generic to avoid inadvertently affecting
others' code.
