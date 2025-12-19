# Catalog or calculate package metrics

When no object is passed, returns a list of possible metrics. When a
[`pkg`](https://pharmar.github.io/val.meter/reference/pkg.md) object is
provided, return metrics calculated for that package. Metrics are a
subset of all the data calculated over the course of assessing a
package. For access to *all* the internally calculated data, pass
`all = TRUE`.

## Usage

``` r
metrics(x, ..., all = FALSE)
```

## Arguments

- x:

  Optionally, an object to retrieve metrics from. When `NULL` (the
  default), a listing of metric metadata is returned.

- ...:

  Additional arguments unused.

- all:

  If `TRUE`, include non-metric package data. These are often
  intermediate data used in the calculation of package metrics.

## Value

A `list` of calculated values or metadata, in the cases where an object
is or is not provided respectively.

## Metrics

The following metrics are provided by
[`val.meter`](https://pharmar.github.io/val.meter/reference/val.meter-package.md).

### Up to date NEWS

`<logical>` a NEWS file exists and is synced with the current package
version

### Total Vignettes

`<integer>` total number of vignettes

### R CMD check Error Count

`<integer>` the number of errors produced when running `R CMD check`

![\[execution\]](figures/badge-req-execution-x-flat-square-red.svg)![\[rcmdcheck\]](figures/badge-dep-rcmdcheck-x-flat-square-green.svg)

[![\[execution\]](figures/badge-execution-x-flat-square-blue.svg)](https://pharmar.github.io/val.meter/reference/tags.md)

### Test coverage of package code by line

`<double>` The fraction of lines of package code that are evaluated by
any test

![\[execution\]](figures/badge-req-execution-x-flat-square-red.svg)![\[covr\]](figures/badge-dep-covr-x-flat-square-green.svg)

### Test coverage of package code by expression

`<double>` The fraction of expressions of package code that are
evaluated by any test

![\[execution\]](figures/badge-req-execution-x-flat-square-red.svg)![\[covr\]](figures/badge-dep-covr-x-flat-square-green.svg)

### Total Downloads

`<integer>` total number of lifetime downloads, as reported by the Posit
CRAN mirror through the [cranlogs](https://cranlogs.rpkg.org) API

![\[network\]](figures/badge-req-network-x-flat-square-red.svg)

[![\[adoption\]](figures/badge-adoption-x-flat-square-blue.svg)](https://pharmar.github.io/val.meter/reference/tags.md)
[![\[transient\]](figures/badge-transient-x-flat-square-blue.svg)](https://pharmar.github.io/val.meter/reference/tags.md)
[![\[version-independent\]](figures/badge-version--independent-x-flat-square-blue.svg)](https://pharmar.github.io/val.meter/reference/tags.md)

### Dependency Count

`<integer>` the number of required dependencies

[![\[best
practice\]](figures/badge-best_practice-x-flat-square-blue.svg)](https://pharmar.github.io/val.meter/reference/tags.md)

### Has Website

`<logical>` a logical indicating whether the package has a website
