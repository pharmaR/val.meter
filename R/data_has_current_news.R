
#' @include impl_data.R
impl_data(
  "has_current_news",
  metric = TRUE,
  class = class_logical,
  title = "Up to date NEWS",
  description = "a NEWS file exists and is synced with the current package version",
  overwrite=TRUE
)


#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom httr2 request req_perform resp_body_html
impl_data(
  "has_current_news",
  for_resource = cran_repo_resource,
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
    html_nodes <- lapply(
      news_lst,
      xml2::xml_find_all,
      sprintf("//text()[contains(., '%s')]",
        gsub("(\\.0)+$", "", as.character(pkg$version)) # remove trailing ".0"
      )
    )
    news_current <- vapply(html_nodes, function(i) length(i) > 0, logical(1L))
    has_news & news_current
  }
)


#' @importFrom tools file_ext
impl_data(
  "has_current_news",
  for_resource = new_union(install_resource, source_code_resource),
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
            content[[i]] <- .tools()$.news_reader_default(f)
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
    # Derive has_news & read news files to see if latest version is present
    has_news <- length(news_lst) > 0
    news_current <- gsub("(\\.0)+$", "", as.character(pkg$version)) |>
      grepl(news_lst)
    has_news & news_current
  }
)


impl_data(
  "has_current_news",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) sample(c(TRUE, FALSE), 1)
)
