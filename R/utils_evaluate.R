#' @importFrom utils capture.output
#' @export
format.evaluate_evaluation <- function(
  x,
  ...,
  style = c("text", "ansi", "html")
) {
  style <- match.arg(style)

  if (
    identical(style, "html") && !requireNamespace("htmltools", quietly = TRUE)
  ) {
    stop(
      "html formatting of evaluation logs requires suggested package ",
      "`htmltools`"
    )
  }

  out <- utils::capture.output(evaluate::replay(x))
  switch(
    style,
    text = paste(cli::ansi_strip(out), collapse = "\n"),
    ansi = paste(out, collapse = "\n"),
    html = {
      html <- cli::ansi_html(paste(out, collapse = "\n"))
      htmltools::tags$div(
        style = cli::ansi_html_style(colors = 8L),
        htmltools::tags$pre(htmltools::HTML(html))
      )
    }
  )
}
