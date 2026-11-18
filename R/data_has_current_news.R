
#' @include impl_data.R

#' Version strings a NEWS entry may use for a package version
#'
#' The full version, plus shorter forms with trailing `.0` components dropped
#' (`"1.2.0"` may be written `"1.2"`), never shorter than `major.minor` so that
#' `"2.0.0"` is not reduced to a bare `"2"`.
#'
#' @param version Package version, as a string or [`package_version`].
#' @return Character vector of version strings, longest first.
#' @noRd
news_version_forms <- function(version) {
  parts <- unlist(package_version(as.character(version)))
  forms <- paste(parts, collapse = ".")
  while (length(parts) > 2L && parts[[length(parts)]] == 0L) {
    parts <- parts[-length(parts)]
    forms <- c(forms, paste(parts, collapse = "."))
  }
  forms
}

#' Does NEWS text mention a package version?
#'
#' Matches each form from `news_version_forms()` literally, as a whole version:
#' `"1.2.1"` does not match `"1.2.10"`, `"11.2.1"` or `"1a2b1"`. A development
#' version (a fourth component of 9000 or more, by the usethis convention) also
#' matches a "development version" heading.
#'
#' @param text Character vector of NEWS text.
#' @param version Package version, as a string or [`package_version`].
#' @return `TRUE` if any line mentions the version.
#' @noRd
news_mentions_version <- function(text, version) {
  text <- as.character(unlist(text))
  forms <- news_version_forms(version)
  escaped <- gsub("([.])", "\\\\\\1", forms)
  # not preceded by a digit or dot, not followed by a digit or ".<digit>"
  pattern <- sprintf(
    "(?<![0-9.])(%s)(?![0-9]|\\.[0-9])",
    paste(escaped, collapse = "|")
  )
  if (any(grepl(pattern, text, perl = TRUE))) {
    return(TRUE)
  }

  parts <- unlist(package_version(as.character(version)))
  is_dev <- length(parts) >= 4L && parts[[4L]] >= 9000L
  is_dev && any(grepl("development version", text, ignore.case = TRUE))
}

impl_data(
  "has_current_news",
  metric = TRUE,
  class = class_logical,
  title = "Up to date NEWS",
  description =
    "a NEWS file exists and is synced with the current package version",
  overwrite = TRUE
)

#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom httr2 request req_perform resp_body_html
impl_data(
  "has_current_news",
  for_resource = cran_repo_resource,
  overwrite = TRUE,
  function(pkg, resource, field, ...) {
    # scrape CRAN html for NEWS link(s)
    news_links <- xml2::xml_find_all(pkg$web_html, xpath = '//a[.="NEWS"]')
    # Grab NEWS link url(s)
    news_urls <-
      sprintf("%s/%s", pkg$web_url,
              vapply(xml2::xml_attrs(news_links), "[", character(1L), "href"))
    # Compile news files into list
    news_lst <-
      lapply(news_urls, function(news_url) {
        request <- httr2::request(news_url)
        response <- httr2::req_perform(request)
        httr2::resp_body_html(response)
      })
    # Derive has_news & read news files to see if latest version is present
    has_news <- length(news_lst) > 0
    news_text <- lapply(news_lst, function(doc) {
      xml2::xml_text(xml2::xml_find_all(doc, "//text()"))
    })
    has_news && news_mentions_version(news_text, pkg$version)
  }
)


#' @importFrom tools file_ext
impl_data(
  "has_current_news",
  for_resource = new_union(source_code_resource, install_resource),
  overwrite = TRUE,
  function(pkg, resource, field, ...) {
    files <- resource@path |>
      list.files(pattern = "^NEWS($|\\.)", full.names = TRUE)
    # Create news_lst by reading and parsing all news.* files
    if (!length(files)) {
      news_lst <- list()
    } else {
      content <- rep(list(NULL), length(files))
      names(content) <- files
      valid <- vector(length(files), mode = "logical")
      # attempt to parse all news.* files
      for (i in seq_along(files)) {
        f <- files[[i]]
        ext <- tools::file_ext(f)
        tryCatch({
          if (tolower(tools::file_ext(f)) == "rd") {
            content[[i]] <- getNamespace("tools")$.news_reader_default(f)
          } else if (tolower(ext) == "md" || nchar(ext) == 0L) {
            # NOTE: should we do validation of markdown format?
            content[[i]] <- readLines(f, warn = FALSE)
          }
          valid[[i]] <- TRUE
        }, error = function(e) {
          valid[[i]] <- FALSE
        })
      }
      news_lst <- content[valid]
    }
    # Derive has_news & read news files to see if latest version is present.
    # Rd NEWS is parsed into a data frame of entries; use its Version column.
    has_news <- length(news_lst) > 0
    news_text <- lapply(news_lst, function(x) {
      if (is.data.frame(x)) as.character(x$Version) else x
    })
    has_news && news_mentions_version(news_text, pkg$version)
  }
)


impl_data(
  "has_current_news",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) sample(c(TRUE, FALSE), 1)
)
