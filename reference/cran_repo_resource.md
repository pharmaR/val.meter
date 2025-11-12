# Package CRAN Repository Resource Class

A reference to a listing in CRAN. CRAN Resources must be from one of the
repositories listed in
[`utils::getCRANmirrors()`](https://rdrr.io/r/utils/chooseCRANmirror.html).
To assert that a new url is also a CRAN mirror, please follow
instructions in
[`utils::getCRANmirrors()`](https://rdrr.io/r/utils/chooseCRANmirror.html).

## Usage

``` r
cran_repo_resource(
  package = NA_character_,
  version = NA_character_,
  id = next_id(),
  md5 = NA_character_,
  repo = character(0)
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

- repo:

  `character(1L)` The repository url from which the package is to be
  sourced.

## See also

Other resources:
[`git_resource()`](https://pharmar.github.io/val.meter/reference/git_resource.md),
[`install_resource()`](https://pharmar.github.io/val.meter/reference/install_resource.md),
[`local_resource()`](https://pharmar.github.io/val.meter/reference/local_resource.md),
[`local_source_resource()`](https://pharmar.github.io/val.meter/reference/local_source_resource.md),
[`mock_resource()`](https://pharmar.github.io/val.meter/reference/mock_resource.md),
[`multi_resource()`](https://pharmar.github.io/val.meter/reference/multi_resource.md),
[`remote_resource()`](https://pharmar.github.io/val.meter/reference/remote_resource.md),
[`repo_resource()`](https://pharmar.github.io/val.meter/reference/repo_resource.md),
[`resource()`](https://pharmar.github.io/val.meter/reference/resource.md),
[`source_archive_resource()`](https://pharmar.github.io/val.meter/reference/source_archive_resource.md),
[`source_code_resource()`](https://pharmar.github.io/val.meter/reference/source_code_resource.md),
[`unknown_resource()`](https://pharmar.github.io/val.meter/reference/unknown_resource.md)
