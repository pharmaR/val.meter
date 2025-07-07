# `val.meter`

_Work-in-progress exploration of a `{riskmetric}` alternative, leveraging S7._

This refactor is inspired by the _R Validation Hub_ Developer Day discussions
and aims to explore some of the discussed design goals.

## Status

- [x] Metric as subclasses of atomic base types
- [x] Metric derivations registered as an S7 generic (`pkg_data_derive`). We
      continue to implement `$` and `[[` operator methods for convenience.
- [x] Metric tags for annotation
- [x] Metric descriptions for annotation
- [x] Metric scopes: required capabilities which must be enabled to calculate
      metrics
- [x] Metric suggests dependencies
- [x] Metric return type checking
- [x] Surfacing of errors from dependent data
- [x] Comprehensive evaluation of all metrics
- [x] Better CLI for communicating about metrics
- [x] Package resource (source of package information) conversion (ie, with a
      CRAN resource, download the source code and create a local source
      code resource)
- [ ] Lots of documentation to write
  - [ ] Function docs
  - [ ] Vignettes
    - [ ] Developer-focused architecture introduction
    - [ ] Adding data and metrics
    - [ ] Extending `val.meter`

## Demo

```r
library(val.meter)

# see all implemented metrics
metrics()

# see all implemented _data_ (a superset of metrics)
metrics(all = TRUE)
```

By default, evaluation uses a very restrictive set of permissions, disallowing
arbitrary code execution and downloading content from the web.

```r
# calculate some data, using default conservative permissions
p <- pkg("../val.meter")

# package data is accessed using indexing operators
p$version
p$downloads_grand_total

# view all metrics (will report execution errors for metrics that can not
# be derived without more permission, ie execution of code and network access)
p@metrics
```

We can opt-in to more extensive capabilities by giving our package more
permissive scopes. We pass `permissions(TRUE)` to grant blanket permission
to all capabilities.

```r
# calculate some data, using permissive execution capabilities
p <- pkg("../val.meter", scopes = permissions(TRUE))

# view all metrics (which will use extended permissions to execute R CMD check
# and query cranlogs API)
p@metrics
```

Just like `riskmetric`, this causes the lazy evaluation of
`rcmdcheck::rcmdcheck`. See `R/data_r_cmd_check.R` to see how metrics
are implemented and a rough overview of the metadata that comes with each
metric.

Now that we've calculated our metrics, we can write them out to a `.dcf` file
conformant with the `PACKAGES` file used to host repositories:

```r
dcf <- to_dcf(p)
cat(dcf, "\n")
```

The object can be _partially_ reconstructed from the `PACKAGES` file contents.
Some information is lost in transit, such as intermediate data derived to
produce these metrics and more full error types captured during execution.

```r
pkg_from_dcf(dcf)
```

We can simulate a cohort of packages using `random_pkgs()`

```r
ps <- random_pkgs(n = 3, scopes = permissions(TRUE))
dcfs <- vapply(ps, to_dcf, character(1L))
cat(dcfs, sep = "\n\n")
```


Now we can easily use this package's derived metrics for activities like
evaluating selection criteria and reporting.

## Explored Features

### Caching, Assessments, Metrics and Scores - Oh, my!

`riskmetric` historically followed a process by which data flowed through
a few different stages before being "scored", resulting in criteria evaluated
on a 0-to-1 scale.

This package drastically simplifies this structure. We search for package
_data_ - that's it!

Some data is an intermediate piece of information needed to derive other, more
meaningful outputs. For example, the result of running `rcmdcheck`, which
returns a structured list.

The data we care most about are classified as "metrics", and _must_ be an
atomic type (with perhaps a couple exceptions in the future for things like
errors). Giving these a specific class will let us define things like
descriptions, tag them with metadata, enforce a type constraint. However,
they're just data and they can be worked with just like any other data.

### Policies & Scopes

I experimented with structuring behaviors as "policies" (behaviors) and
"scopes" (permissions). When searching for packages, we can provide a policy
which constrains how we discover packages. For example, we can prohibit
local installs if we just want what's currently on CRAN, or prohibit any remote
sources if we just want to check our local library's packages.

Scopes apply to package data and metrics and are a way for tagging data with
specific capabilities that are required for their derivation. For example, we
can tag a piece of package data as being transient and users would have to
opt-in to this non-reproducible metric.

## Design Notes

### `S7`

Really nice to work with, aside from collation being a complete pain to manage.
With many classes that are interdependent, they must be defined in the order
of use, which is not necessarily the order that is most intuitive for a
newcomer to the codebase.

In fact, I would argue this structure requires that classes are defined in the
exact opposite order than what would be most approachable to a new developer
- starting with the most nested data structure and building up instead of
starting from the surface of the class hierarchy and peeling back layers of
the architecture.

### Registering Data/Metrics

Data and metrics _could_ be individual classes with properties that store all
the information we need like descriptions, tags, scopes, errors,
(execution logs?, etc. etc.). However, we also want end users to have access
to as much of this information as possible, whether the metric is derived
in this session or written out to a file and read back in.

Information like the description and tags won't change - they're a property of
the _type_, not the _data_. For this reason, we use method dispatch to return
this information, meaning we only need the metric class and we can re-discover
this mapping.

However, this makes it error-prone to register a new metric since one may easily
forget to implement these other supporting methods. For this purpose, a function
is provided which takes all the contextual information and implements all the
methods in one pass.

My hope is that this also makes it easier for supporting or custom packages
to implement their own metrics.

An alternative implemntation might use a S7 class to wrap the derive method
function, which would let us bind this information directly to the function.
This felt like a less intuitive design pattern, even if it models the problem
more directly.

### Metric Suggests Dependencies

With such a broad scope and with so many different preferred dependencies
for gathering package metadata, we will easily amass a large dependency
footprint. In the past we've had to be selective about our dependencies and
have left exotic metrics to community-maintained support packages which never
materialized.

Although I think a supporting package should be able to register metrics,
history has shown us that this almost never becomes a reality. Instead, I'd
like to make it easy to add optional metrics which are only executed when
opt'ed into and when all suggested dependencies are installed, allowing us to
implement these more exotic metrics without relying on external packages.

For this feature to be intuitive, I think it should:

- Be opt-in by default, not run extra metrics so that default metrics are not
  dependent installed packages.
- Be transparent about which packages are missing to enable new metrics.
  Perhaps through a startup message.
- Be transparent about why metrics failed to be computed. Perhaps with a
  specific error type.
- Be easy to enable, perhaps with a helper that allows users to install
  suggested packages based on which metrics they hope to enable.

## S7 Wishlist

Not relevant to this work, just notes about S7 in general that would make it
easier to use:

1. export `register_method`. Using assign syntax with `S7::method<-` can be
   annoying to use programmatically. Would be much easier to use
  `register_method` directly.

2. Somehow lazily evaluate method and class creation during package build so
   that files don't need to be meticulously collated.

3. Check class conformance against class unions.

4. The default `convert()` implementation for upcasting assumes that the
   parent object is an S7 object, not a `base` class. Throws error when trying
   to access properties which don't exist in base classes. Requires overwriting
   existing generics, which emits a lot of noise at build time.
