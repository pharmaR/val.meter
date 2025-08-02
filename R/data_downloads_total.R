#' @include impl_data.R

#' @importFrom jsonlite read_json
impl_data(
  "downloads_total",
  metric = TRUE,
  class = class_integer,
  tags = c("adoption", "transient", "version-independent"),
  permissions = c("network"),
  suggests = c(),
  title = "Total Downloads",
  description = paste0(
    "total number of lifetime downloads, as reported by the Posit ",
    "CRAN mirror through the \\href{https://cranlogs.rpkg.org}{cranlogs} API"
  ),
  function(pkg, resource, ...) {
    from <- as.Date("1970-01-01")
    to <- as.Date("3000-01-01")

    file <- tempfile(paste0(pkg$name, "_downloads"), fileext = ".json")
    url <- sprintf(
      "https://cranlogs.r-pkg.org/downloads/total/%s:%s/%s",
      from, to, pkg$name
    )

    # TODO: handle possible download failure
    downloads <- jsonlite::read_json(url)[[1]]$downloads

    # handle case when package isn't in repo (returns "null")
    tryCatch(as.integer(downloads), warning = function(w) 0L)
  }
)

impl_data(
  "downloads_total",
  for_resource = mock_resource,
  function(...) as.integer(rpois(1, 100)^runif(1, 0.5, 3))
)
