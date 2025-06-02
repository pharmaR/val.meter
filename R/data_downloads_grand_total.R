#' @include trait_pkg_data.R

impl_data(
  "downloads_grand_total",
  metric = TRUE,
  class = class_integer,
  tags = c("adoption", "transient", "version-independent"),
  permissions = c("network"),
  description = paste0(
    "total number of lifetime downloads, as reported by the Posit ",
    "CRAN mirror through the cranlogs.rpkg.org API"
  ),
  function(field, pkg, resource, ...) {
    from <- as.Date("1970-01-01")
    to   <- as.Date("3000-01-01")

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
