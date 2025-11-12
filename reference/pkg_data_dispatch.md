# Various helpers for using a field name for method dispatch

Given a field as a name, for example `"desc"`, we want to dispatch to
the right derivation method for this piece of data. To do this, we need
to convert this field name into an object that has a class that we can
use for method dispatch. These functions are helpers for standardizing
this conversion so that we don't need to concern ourselves with
consistent class names throughout this package.

Given a field as a name, for example `"name"`, we want to dispatch to
the right derivation method for this piece of data. To do this, we need
to convert this field name into an object that has a class that we can
use for method dispatch. These functions are helpers for standardizing
this conversion so that we don't need to concern ourselves with
consistent class names throughout this package.

This is used for finding all available metrics for use in
[`metrics()`](https://pharmar.github.io/val.meter/reference/metrics.md),
as well as for tab completions for `<pkg>$ <TAB>` to auto-populate a
list of available metrics.

## Usage

``` r
pkg_data_s3_class(field_name = NULL, mock = FALSE)

pkg_data_class(...)

pkg_data_name_from_s3_class(class_name)

as_pkg_data(field_name)

get_data_derive_field_names(..., args = list(...))
```

## Arguments

- ...:

  A list of
  [`S7::S7_object`](https://rconsortium.github.io/S7/reference/S7_object.html)
  classes. Not used if `args` is provided.

- args:

  A list of
  [`S7::S7_object`](https://rconsortium.github.io/S7/reference/S7_object.html)
  classes, by default, collects the elements of `...`.

## Value

A `character` vector of field names.

## Details

In most cases, to dispatch to a method for a field, one would

    fn(as_pkg_data("field_name"), ...)

Which creates an `S3` object with a corresponding class. For generics in
this package that might be dispatched by field name, they often have
default methods already implemented so that you can simplify this to:

    fn("field_name", ...)

And it will implicitly dispatch to the appropriate method.

In most cases, to dispatch to a method for a field, one would

    fn(as_pkg_data("field_name"), ...)

Which creates an `S3` object with a corresponding class. For generics in
this package that might be dispatched by field name, they often have
default methods already implemented so that you can simplify this to:

    fn("field_name", ...)

And it will implicitly dispatch to the appropriate method.

## Functions

- `pkg_data_s3_class()`: Convert a field name into an S3 class name for
  dispatch

- `pkg_data_class()`: Convert a field name into an S7 S3 class object
  for dispatch

- `pkg_data_name_from_s3_class()`: Parse a data field name from its S3
  class. The inverse of `pkg_data_s3_class`.

- `as_pkg_data()`: Convert a field name an object with appropriate class
  for dispatch
