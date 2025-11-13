# Package Evaluation Policy Class

A descriptor of how package resources should be discovered, indicating
which types of package resources should be considered and how they must
be used to produce consistently sourced information.

## Usage

``` r
policy(
  accepted_resources = list(
    source_archive_resource,
    source_code_resource,
    install_resource
  ),
  source_resources = list(
    cran_repo_resource,
    repo_resource
  ),
  permissions = class_permissions(FALSE)
)
```

## Arguments

- accepted_resources:

  A list of resources types to permit. Ordered by priority, highest to
  lowest.

- source_resources:

  A list of additional resource types, which may be used to discover a
  resource of an accepted type. For example, even if only
  [`source_archive_resource()`](https://pharmar.github.io/val.meter/reference/source_archive_resource.md)s
  are accepted, a
  [`repo_resource()`](https://pharmar.github.io/val.meter/reference/repo_resource.md)
  could be used as a means of acquiring the archive source, so long as
  it can be
  [`S7::convert()`](https://rconsortium.github.io/S7/reference/convert.html)ed
  into a
  [`source_archive_resource()`](https://pharmar.github.io/val.meter/reference/source_archive_resource.md).

- permissions:

  Behavioral permissions provided for resource acquisition. For example,
  downloading and installing source code for more accurate metric
  evaluation requires the `"network"` and `"write"` permissions.

## Details

The policy takes effect when packages are passed to
[`pkg()`](https://pharmar.github.io/val.meter/reference/pkg.md),
limiting how package resources can be discovered. A policy can be
applied globally using provided
[`options`](https://pharmar.github.io/val.meter/reference/options.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# discover locally installed file path, create `pkg` from `local_resource`
pkg(find.package("val.meter"))

# disable local resource discovery
options(val.meter.policy = policy(
  accepted_resources = list(source_archive_resource)
))

# expect error - unable to discover resource
tryCatch(
  pkg(find.package("val.meter")),
  error = function(error, ...) message(error$message)
)
} # }
```
