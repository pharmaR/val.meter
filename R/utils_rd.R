#' Rd utilities
#'
#' These functions are used primarily for generating documentation
#' programmatically - most notably for documenting metrics that are already
#' documented using [`pkg_data_info`] metadata objects.
#'
#' @keywords internal
#' @include utils.R
#' @name utils-rd
NULL

#' @describeIn utils-rd
#' Create an empty Rd object
rd_empty <- function() {
  structure(list(), class = "Rd")
}

#' @describeIn utils-rd
#' Escape an Rd string
rd_escape <- function(text) {
  text <- gsub(strrep("\\", 2), strrep("\\", 4), text)
  text <- gsub("\"", "\\\"", text)
  text <- gsub("%", "\\%", text, fixed = TRUE)
  text
}

#' @describeIn utils-rd
#' Convert Rd to text-formatted character vector
rd_to_txt <- function(...) {
  out <- character(0L)
  con <- textConnection("out", open = "w", local = TRUE)
  on.exit(close(con))
  tools::Rd2txt(..., out = con)
  out
}

#' @describeIn utils-rd
#' Parse text into an Rd format
rd_parse <- function(text, fragment = FALSE, permissive = TRUE, ...) {
  rds <- tools::parse_Rd(
    file = textConnection(text),
    fragment = fragment,
    permissive = permissive,
    ...
  )

  # remove injected whitespace for character vectors
  if (length(text) > 1) {
    attrs <- attributes(rds)
    rds <- rds[seq_along(text) %% 2]
    attributes(rds) <- attrs
  }

  rds
}

#' @describeIn utils-rd
#' Deparse an Rd object into a character value
rd_deparse <- function(rd, deparse = TRUE, ...) {
  paste0(collapse = "", as.character(rd, deparse = deparse, ...))
}

#' @describeIn utils-rd
#' Convert R code to an Rd \\Sexpr
rd_sexpr <- function(
  code,
  stage = c("build", "install", "render"),
  results = c("text", "verbatim", "rd", "hide"),
  quote = TRUE
) {
  if (quote) code <- substitute(code)
  stage <- match.arg(stage)
  results <- match.arg(results)
  code <- rd_escape(paste(collapse = " ", deparse(code, width.cutoff = 500)))
  paste0("\\Sexpr[stage=", stage, ",results=", results, "]{", code, "}")
}

#' @describeIn utils-rd
#' Generate a badge, using shields.io and caching svg images for display in
#' html output.
rd_badge <- local({
  cache <- NULL

  cache_badge <- function(url, filename) {
    is_roxygenizing <- requireNamespace("roxygen2", quietly = TRUE) &&
      !is.null(roxygen2::roxy_meta_get("env"))

    if (is_roxygenizing) {
      figures_path <- file.path("man", "figures")

      if (is.null(cache)) {
        tmp_cache_dir <- tempfile(paste0(packageName(), "-badge-cache"))
        dir.create(tmp_cache_dir)

        badge_files <- list.files(
          figures_path,
          pattern = "^badge",
          full.names = TRUE
        )

        file.copy(badge_files, tmp_cache_dir)
        cache <<- tmp_cache_dir
      }

      if (!dir.exists(figures_path)) {
        dir.create(figures_path, recursive = TRUE)
      }

      n <- max(length(url), length(filename))
      url <- rep(url, length.out = n)
      filename <- rep(filename, length.out = n)

      for (i in seq_len(n)) {
        if (file.exists(filename[[i]])) {
          next
        }

        if (file.exists(cached <- file.path(cache, filename[[i]]))) {
          file.copy(cached, file.path(figures_path, filename[[i]]))
          next
        }

        download.file(
          url = url[[i]],
          destfile = file.path(figures_path, filename[[i]])
        )
      }
    }
  }

  badge_encode <- function(text) {
    text <- gsub(" ", "_", text)
    text <- gsub("-", "--", text)
    text
  }

  function(
    message,
    label = "",
    style = "",
    dest = NULL,
    color = "blue",
    url = "https://img.shields.io/badge/",
    params = list(style = "flat-square")
  ) {
    if (length(message) < 1L) {
      return(character(0L))
    }

    svg_slug <- paste0(
      badge_encode(label),
      ifelse(nchar(label), "-", ""),
      badge_encode(message),
      "-x"
    )

    params$color <- color
    params_str <- paste0(collapse = "&", names(params), "=", params)
    svg_url <- paste0(url, svg_slug, "?", params_str)

    svg_filename <- paste0(
      "badge-",
      svg_slug,
      if (length(params) > 0) "-",
      paste(params, collapse = "-"),
      ".svg"
    )

    cache_badge(svg_url, svg_filename)

    html <- rd_figure(svg_filename, alt = paste0("[", message, "]"))
    text <- paste0(
      "\\strong{[",
      label, ifelse(nzchar(label), "::", ""), message,
      "]}"
    )

    content <- rd_ifelse("html", html, text)

    switch(
      style,
      link = rd_sexpr(
        stage = "build",
        results = "rd",
        quote = FALSE,
        bquote(
          if (
            numeric_version(paste0(R.version$major, ".", R.version$minor)) <
              "4.5.0"
          ) {
            .(paste(collapse = "\n", rd_link(content, dest = dest)))
          } else {
            .(paste(collapse = "\n", content))
          }
        )
      ),
      href = rd_href(content, dest = dest),
      content
    )
  }
})

#' @describeIn utils-rd
#' Generate `\figure{}` Rd output
rd_figure <- function(filename, alt, options = list(alt = alt)) {
  # vectorize for option lengths
  length.out <- max(viapply(options, length))
  options <- lapply(options, rep_len, length.out = length.out)
  options <- lapply(
    seq_len(length.out),
    function(n) lapply(options, function(x, i) x[[i]], i = n)
  )

  # build vector of option strings
  options_strs <- vcapply(
    options,
    function(x) paste(collapse = " ", names(x), "=", vcapply(x, deparse))
  )

  paste0("\\figure{", filename, "}", "{options: ", options_strs, "}")
}

#' @describeIn utils-rd
#' Generate `\ifelse{}` Rd output
rd_ifelse <- function(condition, true, false) {
  paste0("\\ifelse{", condition, "}", "{", true, "}", "{", false, "}")
}

#' @describeIn utils-rd
#' Generate `\href{}` Rd output
rd_href <- function(content, dest) {
  paste0("\\href{", dest, "}", "{", content, "}")
}

#' @describeIn utils-rd
#' Generate `\link[]{}` Rd output
rd_link <- function(content, dest) {
  if (!missing(dest)) dest <- paste0("[", dest, "]")
  paste0("\\link", dest, "{", content, "}")
}

#' Support rendering metrics to Rd
#'
#' @note Unlike [`tools::toRd`], methods return a `character(0L)` because
#' [`tools::toRd`] and [`tools::parse_Rd`] do not round-trip escape characters
#' properly, making it challenging to compose Rd files from multiple return
#' objects.
#'
#' A more effective implementation would require avoiding character
#' intermediates altogether.
#'
#' @importFrom tools toRd
#' @noRd
new_external_generic("tools", "toRd", "obj")

#' @include class_tags.R
method(toRd, class_tags) <- function(obj, ...) {  # nolint: object_name_linter.
  dest <- paste0(packageName(), ":tags")
  paste(collapse = "\n", rd_badge(obj, dest = dest, style = "link"))
}

#' @include class_permissions.R
method(toRd, class_permissions) <- function(obj, ...) {  # nolint: object_name_linter, line_length_linter.
  dest <- paste0("[", packageName(), ":permissions]")

  scope_badge <- function(permission, color) {
    rd_badge(label = "req", permission, color = color, dest = dest)
  }

  paste(collapse = " ", vcapply(obj, function(permission) {
    rd_sexpr(stage = "render", results = "rd", quote = FALSE, bquote(
      if (!is.na(match(.(permission), getOption("val.meter.permissions"))))
        .(scope_badge(permission, "green"))
      else
        .(scope_badge(permission, "red"))
    ))
  }))
}

#' @include class_suggests.R
method(toRd, class_suggests) <- function(obj, ...) {  # nolint: object_name_linter, line_length_linter.
  dest <- paste0("[", packageName(), ":permissions]")

  suggests_badge <- function(pkg, color) {
    rd_badge(label = "dep", pkg, color = color, dest = dest)
  }

  paste(collapse = " ", vcapply(obj, function(suggest) {
    rd_sexpr(stage = "render", results = "rd", quote = FALSE, bquote(
      if (length(find.package(.(suggest), quiet = TRUE)) > 0)
        .(suggests_badge(suggest, "green"))
      else
        .(suggests_badge(suggest, "red"))
    ))
  }))
}
