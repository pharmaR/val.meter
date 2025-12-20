#' @importFrom httr2 request req_perform resp_body_html
#' @include impl_data.R

impl_data(
  "web_url",
  class = class_character,
  for_resource = cran_repo_resource,
  function(pkg, resource, field, ...) {
    sprintf(
      "%s/web/packages/%s",
      resource@repo,
      resource@package
    )
  }
)

impl_data(
  "web_html",
  class = c("xml_document", "xml_node"),
  for_resource = cran_repo_resource,
  permissions = "network",
  function(pkg, resource, field, ...) {
    req <- httr2::request(pkg$web_url)
    resp <- httr2::req_perform(req)
    httr2::resp_body_html(resp)
  }
)
