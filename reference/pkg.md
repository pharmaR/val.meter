# Create a new package object to start measuring package heuristics

Aggregates package information, surveying metadata from discovered
resources.

## Usage

``` r
pkg(resource, permissions, policy = opt("policy"))
```

## Arguments

- resource:

  [`resource`](https://pharmar.github.io/val.meter/reference/resource.md)
  (often a
  [`multi_resource`](https://pharmar.github.io/val.meter/reference/multi_resource.md)),
  providing the resources to be used for deriving packages data. If a
  [`multi_resource`](https://pharmar.github.io/val.meter/reference/multi_resource.md),
  the order of resources determines the precedence of information. If
  information about a package could be derived from multiple sources,
  the first source is prioritized.

- permissions:

  [`permissions`](https://pharmar.github.io/val.meter/reference/permissions.md)
  granted for deriving data. If not provided, the default from `policy`
  will be used.

- policy:

  [`policy`](https://pharmar.github.io/val.meter/reference/policy.md) to
  use when converting input to resources. Most commonly used for
  interpreting strings as resources. If `permissions` is specified it
  will mask the permissions provided in `policy`.
