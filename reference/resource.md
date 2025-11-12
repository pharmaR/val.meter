# Resource Class

A package resource is a resource for producing package information.
Package resources can vary from source code repositories to R package
repository listings. Each resources should be able to produce a
downloadable version of executable package code, but the extent of
included code might vary depending on source.

## Usage

``` r
resource(
  package = NA_character_,
  version = NA_character_,
  id = next_id(),
  md5 = NA_character_
)
```

## Arguments

- package:

  `character(1L)` Package name. Optional, but should be provided if
  possible.

- version:

  `character(1L)` Package version, provided as a string.

- id:

  `integer(1L)` optional id used for tracking resources throughout
  execution. Generally not provided directly, as new objects
  automatically get a unique identifier. For example, the package source
  code from a
  [`repo_resource()`](https://pharmar.github.io/val.meter/reference/repo_resource.md)
  may be downloaded to add a
  [`source_archive_resource()`](https://pharmar.github.io/val.meter/reference/source_archive_resource.md)
  and add it to a new
  [`multi_resource()`](https://pharmar.github.io/val.meter/reference/multi_resource.md).
  Because all of these represent the same package, they retain the same
  `id`. Primarily the `id` is used for isolating temporary files.

- md5:

  `character(1L)` md5 digest of the package source code tarball. This is
  not generally provided directly, but is instead derived when acquiring
  resources.

## Details

Package resources should also implement
[`convert()`](https://rconsortium.github.io/S7/reference/convert.html),
providing a method for converting into a more desirable source of
information. For example, given only a listing of a package in a
repository, calling
[`convert()`](https://rconsortium.github.io/S7/reference/convert.html)
on such a reference may populate a local directory with the source code
of the package.

## See also

Other resources:
[`cran_repo_resource()`](https://pharmar.github.io/val.meter/reference/cran_repo_resource.md),
[`git_resource()`](https://pharmar.github.io/val.meter/reference/git_resource.md),
[`install_resource()`](https://pharmar.github.io/val.meter/reference/install_resource.md),
[`local_resource()`](https://pharmar.github.io/val.meter/reference/local_resource.md),
[`local_source_resource()`](https://pharmar.github.io/val.meter/reference/local_source_resource.md),
[`mock_resource()`](https://pharmar.github.io/val.meter/reference/mock_resource.md),
[`multi_resource()`](https://pharmar.github.io/val.meter/reference/multi_resource.md),
[`remote_resource()`](https://pharmar.github.io/val.meter/reference/remote_resource.md),
[`repo_resource()`](https://pharmar.github.io/val.meter/reference/repo_resource.md),
[`source_archive_resource()`](https://pharmar.github.io/val.meter/reference/source_archive_resource.md),
[`source_code_resource()`](https://pharmar.github.io/val.meter/reference/source_code_resource.md),
[`unknown_resource()`](https://pharmar.github.io/val.meter/reference/unknown_resource.md)
