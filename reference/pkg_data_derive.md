# Derive a package data field

Derive a [`pkg`](https://pharmar.github.io/val.meter/reference/pkg.md)
data field, giving a function by which a piece of package data is
calculated. This function is not called directly, it used by the
indexing functions implemented for
[`pkg`](https://pharmar.github.io/val.meter/reference/pkg.md) to
populate new data fields. Default implementations accept `field` as a
`character`, automatically dispatching on the field as a class object,
and accept a missing `resource`, instead iterating through `pkg`
resources by priority.

## Usage

``` r
pkg_data_derive(pkg, resource, field, ...)
```

## Arguments

- pkg:

  A [`pkg()`](https://pharmar.github.io/val.meter/reference/pkg.md)

- resource:

  A
  [`resource()`](https://pharmar.github.io/val.meter/reference/resource.md),
  or if not provided, the
  [`resource`](https://pharmar.github.io/val.meter/reference/resource.md)
  extracted from `pkg@resource`.

- field:

  Used for dispatching on which field to derive. Methods are provided
  such that a simple `character` field name can be passed and used to
  build a class for dispatching to the right derivation function.

- ...:

  Used by specific methods.

## Value

The derived field value.

## Details

This function is used internally when accessing a
[`pkg`](https://pharmar.github.io/val.meter/reference/pkg.md) object
using `$`, `[[` and `[`.

## See also

Other generics:
[`pkg_data_info()`](https://pharmar.github.io/val.meter/reference/pkg_data_info.md)
