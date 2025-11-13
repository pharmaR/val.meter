# Derive package data information

Retrieve metadata about a data field.

## Usage

``` r
pkg_data_info(field, resource, ...)
```

## Arguments

- field:

  `character(1L)` field name to retrieve.

- resource:

  [`resource`](https://pharmar.github.io/val.meter/reference/resource.md)
  providing the resource to be used for retrieving package data info.

- ...:

  Additional arguments unused.

## Value

`data_info` for field `field`.

## See also

Other generics:
[`pkg_data_derive()`](https://pharmar.github.io/val.meter/reference/pkg_data_derive.md)
