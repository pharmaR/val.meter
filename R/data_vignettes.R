#' @importFrom xml2 xml_find_all
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
  function(pkg, resource, field, ...) {
    nrow(tools::getVignetteInfo(pkg$name, dirname(resource@path)))
  }
)

impl_data(
  "vignette_count",
  for_resource = cran_repo_resource,
  function(pkg, resource, field, ...) {
    nodes <- xml2::xml_find_all(
      pkg$web_html,
      xpath = '//a[contains(@href,"vignettes")]'
    )

    if (!length(nodes)) {
      return(0)
    }

    paths <- xml2::xml_attr(nodes, "href")
    filenames <- basename(paths)
    filestems <- tools::file_path_sans_ext(filenames)

    length(unique(filestems))
  }
)

#' @importFrom stats rbinom
impl_data(
  "vignette_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) rbinom(1, 10, .3)
)
