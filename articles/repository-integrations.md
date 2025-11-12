# Repository Integrations

``` css
[data-bs-theme="dark"] main img {
  filter: invert(1);
}
```

`val.meter` is intended to support a variety of use cases, from
individuals who are trying to appraise packages, to organizations trying
to consistently impose a package policy, to serving repositories of
package metadata.

In these examples, we’ll simulate a cohort of packages to support the
testing and deployment of cohort-wide package policies - providing a
representation of an end product without all the work to get there. Here
we’ll build a cohort of generated packages and provide some heuristics
across all of them.

## Generating random packages

`val.meter` provides a few tools for simulation,
[`random_pkg()`](https://pharmar.github.io/val.meter/reference/random_pkg.md)
and
[`random_pkgs()`](https://pharmar.github.io/val.meter/reference/random_pkg.md),
differing only in whether you want to simulate a single or collection of
packages.

``` r
p <- random_pkg(permissions = TRUE)
metrics(p)
#> $vignette_count
#> [1] 3
#> 
#> $r_cmd_check_error_count
#> [1] 0
#> 
#> $downloads_total
#> [1] 690022
#> 
#> $dependency_count
#> [1] 0
#> 
#> $has_website
#> [1] FALSE
```

``` r
sapply(
  random_pkgs(n = 3, permissions = TRUE),
  function(pkg) pkg$name
)
#> [1] "wiseTime" "promotr"  "cheRish"
```

## Generating a repostiroy

`pkg` objects provide an implementation of `to_dcf`, allowing them to be
encoded as a `PACKAGES` file - the same format used by repositories like
CRAN to distribute a listing of packages and package data.

``` r
# igraph is required if we want to simulate sensible package dependencies
requireNamespace("igraph")

# generate some random packages
ps <- random_pkgs(n = 3, permissions = TRUE)

# output DCF files
dcf_str <- to_dcf(ps)
cat(dcf_str, "\n")
#> Package: cozygenerousboom
#> Version: 0.2.5
#> Depends: R
#> Suggests: thrillGrid, biobreeze
#> License: Phony License
#> MD5sum: c919223f198551751b387ec7982d5f81
#> Metric/vignette_count@R: 5
#> Metric/r_cmd_check_error_count@R: 0
#> Metric/downloads_total@R: 161
#> Metric/dependency_count@R: 1
#> Metric/has_website@R: FALSE
#> 
#> Package: thrillGrid
#> Version: 2.3.3
#> Depends: R
#> Suggests: biobreeze
#> License: Phony License
#> MD5sum: 71567f7d47168519de48679a533362a7
#> Metric/vignette_count@R: 3
#> Metric/r_cmd_check_error_count@R: 0
#> Metric/downloads_total@R: 15711
#> Metric/dependency_count@R: 1
#> Metric/has_website@R: FALSE
#> 
#> Package: biobreeze
#> Version: 7.4.1
#> Depends: R
#> License: Phony License
#> MD5sum: 53ede02e6e33623057e5b4ff92e3b2a3
#> Metric/vignette_count@R: 5
#> Metric/r_cmd_check_error_count@R: 1
#> Metric/downloads_total@R: 27978
#> Metric/dependency_count@R: 1
#> Metric/has_website@R: FALSE
```

## Pulling package metadata from a package repository

What’s more, we can also reconstruct our package objects from this
`PACKAGES` file format.

``` r
ps <- pkgs_from_dcf(dcf_str)
```

This exposes the package metadata through a convenient and familiar
interface, allowing full access to metric metadata.

However, these new package objects differ from their original objects in
a few important ways. First, their source isn’t reconstructed. However
you produced the text output, we no longer have an explicit record of
that process. All we know by the time we re-build our package objects is
the hash of the built file from some unknown origin. Second,
intermediate data is not reconstructed. This includes any logs or
computational intermediate data such as full `R CMD check` results. All
we have are the derived metrics. Finally, any errors raised by
`val.meter` are preserved, but their call stacks are lost in this
process. If you do want to preserve rich metadata, it is recommended to
save full data objects for posterity.

## Analysing our repository

Let’s imagine that we want to use this repository metadata to make an
informed decision about packages.

We’ll start by re-simulating a larger cohort of packages so that our
anlaysis produces something more interesting.

``` r
n100pkgs <- random_pkgs(n = 100, permissions = TRUE)
```

And just to show that we can derive this data from a representative
`PACKAGES` file, we’ll write out and read back in our data from text
format.

``` r
dcf <- to_dcf(n100pkgs)
n100pkgs <- pkgs_from_dcf(dcf)
```

Finally, we can take a look at how our packages fair.

``` r
# read in packages as a data.frame
df <- as.data.frame(n100pkgs)

# small helper for calculating percentiles
percentile <- function(x, ...) ecdf(x, ...)(x)

# calculate and filter on package dependency count percentiles
df$dependency_percentile <- percentile(df$dependency_count)

# find our packages with the most dependencies
df$package[df$dependency_percentile > 0.95]
#> [1] "brilliantthankfulmerit" "gorgeouseager"          "geogentleexcitedness"  
#> [4] "ggdistribution"         "geneRalized"            "favouR"
```

``` r
options(scipen = 10)
plot(
  xlab = "Percently",
  x = df$dependency_percentile,
  ylab = "Dependency Count",
  y = df$dependency_count
)
```

![An example of dependency counts distributed over their percentiles,
showcasing a mostly linear character. The plot is randomly generated, so
exact characteristics may
vary.](repository-integrations_files/figure-html/analysis-2-1.svg)
