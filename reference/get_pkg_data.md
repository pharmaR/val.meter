# Get [`pkg`](https://pharmar.github.io/val.meter/reference/pkg.md) object data

This methods handles the error handling and propagation of deriving
package data. It is the primary interface by which a
[`pkg`](https://pharmar.github.io/val.meter/reference/pkg.md) object
should be deriving data. In contrast to
[`pkg_data_derive`](https://pharmar.github.io/val.meter/reference/pkg_data_derive.md),
which is the method that individual data implements to register it as a
field, this function wraps the execution in appropriate error handling
for user presentation. This function should not throw errors, but
instead should capture errors for communication back to the user.

## Usage

``` r
get_pkg_data(x, name, ..., .raise = .state$raise)
```

## Arguments

- x:

  [`pkg`](https://pharmar.github.io/val.meter/reference/pkg.md) object
  to derive data for

- name:

  `character(1L)` field name for the data to derive

- ...:

  Additional arguments unused

- .raise:

  `logical(1L)` flag indicating whether errors should be raised or
  captured. This flag is not intended to be set directly, it is exposed
  so that recursive calls can raise lower-level errors while capturing
  them at the surface.

## Value

the derived data, using the method of
[`pkg_data_derive`](https://pharmar.github.io/val.meter/reference/pkg_data_derive.md)
dispatched on field `name`.
