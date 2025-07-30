# `val.meter`

_validation-ready package quantification supporting regulatory use of R_

## Installation

```r
# install.packages("pak")
pak::pak("pharmaR/val.meter")
```

## Overview

`val.meter` is the central package for collecting package metrics - objective
qualities of R packages. 

Metrics can vary depending on a number of conditions such as the package
version, computing environment where the metric was measured, available
dependencies and possibly external resources like Posit's CRAN mirror downloads
API. `val.meter` gives full control over how you define package sources and
which permissions are enabled when evaluating the package to make this process
as reproducible as possible.

```r
rpkg <- random_pkg()
metrics(rpkg)
```

## Features

### Exploring metrics

`val.meter` makes it easy to find a listing of metrics and details of what those
metrics represent.

```r
library(val.meter)

# read documentation of all metrics
?metrics

# see all implemented metrics programmatically
metrics()
```

`metrics` actually represent a small set of the internally calculated package
data, which may go through a few stages of calculation before resulting in a
quantifiable metric. If you want to see _all_ the internal information, you can
explore the whole set of package data.

`metrics` are unique in that their data is required to be consistent and simple
(atomic) data.

```r
# see all implemented _data_ (a superset of metrics)
metrics(all = TRUE)
```

### Creating package objects

To start evaluating metrics, start by create a `pkg` (package) object. Package
objects represent a set of resources from which metadata is pulled and a
collection of package metadata. When we pass a `character` path or package name,
`val.meter` will search for the package from permitted sources.

```r
# calculate some data, using default conservative permissions
p <- pkg("../val.meter")
```

We can also be declarative about exactly how we want to source our package
metadata.

```r
# initialize package with only our installed package as a resource
resrc <- convert("../val.meter", source_code_resource)
pkg(resrc)
```

#### Calculating metrics

Once we have declared _what_ we want to assess, we can then start calculating
metrics.

```r
metrics(p)
```

In this case, you'll notice that some metric calculations raised errors during
execution. Here you'll notice that we have not granted `val.meter` permission
to execute code or fetch data from APIs over the network. 

We can also access individual metrics by accessing our `pkg` object like a list.

```r
p$dependency_count
```

Packages are _lazy_ :zzz:! Since some metrics can be computationally intensive,
we only calculate them as their needed. After they're calculated the first
time, the result is saved so you can access it whenever you need it.

#### Managing execution permissions

We can opt-in to more extensive capabilities by giving our package more
permissive scopes. We pass `permissions(TRUE)` to grant blanket permission
to all capabilities.

```r
metrics(pkg("../val.meter", scopes = permissions(TRUE)))
```
