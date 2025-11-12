# A matrix produced by `available.packages`

A matrix produced by `available.packages`

## Usage

``` r
class_package_matrix(.data = character(0))
```

## Arguments

- .data:

  See
  [`S7::class_character`](https://rconsortium.github.io/S7/reference/base_classes.html)

## Note

Due to a [limitation of `S7`'s handling of `matrix` and `array`
classes](https://github.com/RConsortium/S7/issues/401), we currently
need to wrap matrices that we want to handle through `convert` in our
own custom classes. This may not be necessary in the future.
