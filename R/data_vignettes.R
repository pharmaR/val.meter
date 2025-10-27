#' @include impl_data.R

impl_data(
  "vignette_count",
  metric = TRUE,
  class = class_integer,
  title = "Total Vignettes",
  description = "total number of vignettes"
)

#' @importFrom tools getVignetteInfo
impl_data(
  "vignette_count",
  for_resource = install_resource,
  function(pkg, resource, ...) {
    nrow(tools::getVignetteInfo(pkg$name, dirname(resource@path)))
  }
)

#' @importFrom stats rbinom
impl_data(
  "vignette_count",
  for_resource = mock_resource,
  function(...) rbinom(1, 10, .3)
)
