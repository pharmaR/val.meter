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
    # pull package downloads
    # NOTE: apparently the only way to get lifetime downloads is by parsing
    #   the badge content?
    url <- paste0("https://cranlogs.r-pkg.org/badges/grand-total/", pkg$name)
    file <- tempfile(paste0(pkg$name, "_badge"), fileext = "svg")

    # TODO: handle possible download failure
    download.file(url, file, quiet = TRUE)

    # extract grand total from svg, will be in human-readable format, eg "1.2K"
    xml <- xml2::read_html(file)
    total_node <- xml2::xml_find_first(xml, "//text[last()]")
    total <- trimws(xml2::xml_text(total_node))

    # parse human-readable format into best estimate of total
    digits <- gsub("[[:alpha:]]", "", total)
    suffix <- substring(total, nchar(digits) + 1L)
    suffix_as_scientific <- switch(suffix, "B" = "9", "M" = "6", "K" = "3")

    # handle case when package isn't in repo (returns "null")
    tryCatch(
      as.integer(paste0(digits, "e+", suffix_as_scientific)),
      warning = function(w) 0L
    )
  }
)
