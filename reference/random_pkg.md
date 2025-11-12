# Generate Random Package(s)

Create a package object to simulate metric derivation. When generating a
collection of packages, dependencies will realistically be made between
packages.

## Usage

``` r
random_pkg(
  package = random_pkg_name(),
  version = random_pkg_version(),
  md5 = md5sum(paste0(package, " v", version)),
  ...,
  permissions = TRUE
)

random_pkgs(n = 100, ...)

random_repo(..., path = tempfile("repo"))
```

## Arguments

- package:

  `character(1L)` a package name

- version:

  `character(1L)` a package version

- md5:

  `character(1L)` an `md5` hash to use for the package. By default, is a
  simple hash derived from the package name and version.

- ...:

  Additional arguments passed to
  [`mock_resource`](https://pharmar.github.io/val.meter/reference/mock_resource.md)

- permissions:

  `permissions` a permissions object, or a anything that can be
  interpretted as a permissions object through `convert`. Unlike general
  [`pkg`](https://pharmar.github.io/val.meter/reference/pkg.md)s,
  `random_pkg`s default to mocking a package with all permissions are
  enabled to mock as many metrics as possible.

- n:

  `integer(1L)` how many packages to simulate

- path:

  `character(1L)` directory path where the repository should be created.
  Directory will be created if it doesn't yet exist.

## Functions

- `random_pkgs()`: Generate a set of random packges

- `random_repo()`: Create a random assortment of packages and write the
  out to local repository file structure such that it can be used with
  `options(repos = random_repo())`
