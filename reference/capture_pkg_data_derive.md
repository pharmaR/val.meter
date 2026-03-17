# Derive data and capture output

Uses
[`evaluate::evaluate`](https://pharmar.github.io/val.meter/reference/evaluate.r-lib.org/reference/evaluate.md)
to capture execution logs.

## Usage

``` r
capture_pkg_data_derive(pkg, resource, field, ..., quiet = opt("quiet"))
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
