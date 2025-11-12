# Package Data

Package data is feature of val.meter - it is the framework by which we
calculate package qualities and derive metrics. Package data is any
derived information about a package. Fundamentally, new data is
implemented using the methods
[`pkg_data_derive()`](https://pharmar.github.io/val.meter/reference/pkg_data_derive.md)
and
[`pkg_data_info()`](https://pharmar.github.io/val.meter/reference/pkg_data_info.md)
used for calculating and providing metadata respectively.

## Usage

``` r
impl_data(
  name,
  fn,
  for_resource = resource,
  ...,
  overwrite = FALSE,
  quiet = FALSE
)

impl_data_info(
  name,
  class = class_any,
  title = character(0L),
  description = character(0L),
  tags = class_tags(c()),
  permissions = class_permissions(character(0L)),
  suggests = class_suggests(character(0L)),
  metric = FALSE,
  overwrite = FALSE,
  quiet = FALSE
)

impl_data_derive(name, fn, for_resource, overwrite = FALSE, quiet = FALSE)
```

## Arguments

- name:

  `character(1L)` the data field name to implement.

- fn:

  `function` to use when deriving the data for a `pkg`.

- for_resource:

  `resource` classes that can use this method to derive data. Classes
  can also be
  [`S7::new_union()`](https://rconsortium.github.io/S7/reference/new_union.html)s
  or abstract classes to allow multiple resources to use the same
  method.

- ...:

  Additional arguments passed from `impl_data` to `impl_data_info` and
  are provided as metadata.

- overwrite:

  `logical(1L)` flag indicating that the method should overwrite an
  existing method that uses the same dispatch arguments.

- quiet:

  `logical(1L)` flag indicating that overwriting should be quiet,
  suppressing messages emitted during overwriting.

- class:

  [`S7::S7_class`](https://rconsortium.github.io/S7/reference/S7_class.html)
  or `character(n)`. A return type that is type checked after method
  evaluation. `character` values will be coerced into `S7`
  representations of `S3` classes.

- title:

  `character(1L)` data title, used for user-facing communication about
  data derivation or purpose.

- description:

  `character(n)` or `Rd` object used as longer-form documentation about
  the data.

- tags:

  [`tags()`](https://pharmar.github.io/val.meter/reference/tags.md)
  associated with the data.

- permissions:

  [`permissions()`](https://pharmar.github.io/val.meter/reference/permissions.md)
  required to compute the data.

- suggests:

  `character(n)` packages which must be installed in order to derive the
  data.

- metric:

  `logical(1L)` flag indicating whether the data should be user-facing
  as a metric. When `TRUE`, `class` must be
  [`atomic`](https://rdrr.io/r/base/vector.html).

## Details

However, implementing these functions directly requires a bit of
knowledge of how the internals of the package are structured. Instead,
it is recommended to use `impl_data()`, which provides a high-level
interface that helps make this process as simple as possible.

## Functions

- `impl_data()`: Helper for implementing all the necessary methods for
  package data. Internally this is a wrapper for `[impl_data_meta()]`
  (associate metadata with the data field), `[impl_data_derive()]`
  (associate a derivation function for a combination of data field *and*
  package resource) and `[impl_metric()]` (declare a piece of data to be
  a metric).

- `impl_data_info()`: Associate metadata with the data field

- `impl_data_derive()`: Register a derivation function for a data field
  and package resource.

## Implementing a new metric

To implement some new data, you can use `impl_data()`, providing, at a
minimum, the field name for the data and the way it should be
calculated.

    impl_data("name_character_count", function(pkg, ...) nchar(pkg$name))

## Examples

``` r
p <- random_pkg()
impl_data("name_character_count", function(pkg, ...) nchar(pkg$name))
p$name_character_count
#> [1] 32

```
