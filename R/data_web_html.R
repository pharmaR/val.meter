#' @include impl_data.R

impl_data(
  "web_url",
  class = class_character,
  for_resource = cran_repo_resource,
  function(pkg, resource, ...) {
    sprintf(
      "%s/web/packages/%s",
      dirname(dirname(resource@repo)),
      resource@package
    )
  }
)

impl_data(
  "web_html",
  class = c("xml_document", "xml_node"),
  for_resource = cran_repo_resource,
  permissions = "network",
  function(pkg, resource, ...) {
    pkg$web_url |>
      httr2::request() |>
      httr2::req_perform() |>
      httr2::resp_body_html()
  }
)
