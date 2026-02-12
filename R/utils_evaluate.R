#' `knitr` Pretty-printing of rich ansi output captured with `evaluate()`
#'
#' @inheritParams knitr::knit_print
#'
#' @export
# nolint start
knit_print.evaluate_evaluation <- function(x, ...) {
  # nolint end
  res <- format_output(evaluate::replay(x))
  if (!is.character(res) && requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_print(res)
  } else {
    cat(res)
  }
}

#' Formatted version of `capture.output()`
#'
#' Captures output and formats it according to `style`. Uses the [evaluate]
#' package to capture output to be more resilient to other processes sinking
#' output, causing issues with `capture.output` -- notably when used with
#' [knitr].
#'
#' @param x An expression to capture output from.
#' @param ... Additional arguments unused.
#' @param style What style to format output as. When `knitr` is running,
#'   infers the preferred format from the output document type.
#' @param evaluate Whether to evaluate `x` by first capturing output with
#'   [evaluate::evaluate]. When `FALSE`, format `x` as a `character` value
#'   directly.
#' @param envir An environment in which expression `x` should be evaluated.
#'
#' @examples
#' format_output(
#'   cli::cli_text(cli::col_red("hello, world!")),
#'   type = "message",  # cli outputs to message stream
#'   style = "html"
#' )
#'
#' @importFrom utils capture.output
#' @export
format_output <- function(
  x,
  ...,
  style = infer_format_style(),
  evaluate = TRUE,
  envir = parent.frame()
) {
  style <- match.arg(style, choices = c("text", "ansi", "html"))

  if (
    identical(style, "html") && !requireNamespace("htmltools", quietly = TRUE)
  ) {
    stop(
      "html formatting of evaluation logs requires suggested package ",
      "`htmltools`"
    )
  }

  old_options <- options(width = 80L, crayon.enabled = TRUE, cli.ansi = TRUE)
  on.exit(options(old_options))

  out <- if (evaluate) {
    fn <- function() {}
    body(fn) <- substitute(x)
    ev <- evaluate::evaluate(fn, envir = envir)[-1L]
    capture.output(evaluate::replay(ev))
  } else {
    x
  }

  switch(
    style,
    text = paste(cli::ansi_strip(out), collapse = "\n"),
    ansi = paste(out, collapse = "\n"),
    html = {
      html <- cli::ansi_html(paste(out, collapse = "\n"))
      singleton <- cli_singleton_html_style(colors = 8L)
      htmltools::div(
        singleton,
        htmltools::tags$pre(
          class = c("r-output", names(singleton)),
          htmltools::HTML(html)
        )
      )
    }
  )
}

#' Try to infer the output format for formatting functions
#'
#' @keywords internal
#'
infer_format_style <- function() {
  if (
    getOption("knitr.in.progress", FALSE) &&
      requireNamespace("knitr", quietly = TRUE)
  ) {
    return(switch(
      knitr::opts_knit$get("rmarkdown.pandoc.to") %||%
        knitr::opts_knit$get("out.format"),
      "html" = "html",
      "text"
    ))
  }

  if (cli::is_ansi_tty()) {
    return("ansi")
  }

  "text"
}

#' Produce a html style for cli output
#'
#' Returns a list with one element, the html style tag, whose name is the
#' class name given to this style.
#'
#' @keywords internal
#'
cli_singleton_html_style <- function(...) {
  args <- match.call(cli::ansi_html_style, expand.dots = TRUE)[-1L]
  css_class <- paste0(
    "r-cli--",
    paste0(names(args), "-", args, collapse = "--")
  )

  style <- format(cli::ansi_html_style(colors = 8L))
  html <- htmltools::singleton(
    htmltools::tags$style(
      paste0(
        "\n",
        paste0(".", css_class, names(style), " ", style, collapse = "\n"),
        "\n"
      )
    )
  )

  html <- list(html)
  names(html) <- css_class

  html
}

#' @importFrom evaluate replay
#' @export
format.evaluate_evaluation <- function(
  x,
  ...
) {
  format_output(evaluate::replay(x), ...)
}
