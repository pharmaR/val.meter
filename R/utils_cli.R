fmt <- function(..., .envir = parent.frame()) {
  cli::format_inline(..., .envir = .envir)
}

#' Create a cli-formatted badge
#' 
#' @param style A `list` of arguments to pass to the ellipsis arguments of
#'   [`cli::make_ansi_style()`].
cli_tag <- function(
  ...,
  scope = NULL,
  color = "blue",
  .envir = parent.frame()
) {
  symbols <- list(
    lbracket = "\U0001FB6E",
    rbracket = "\U0001FB6C",
    rhalfblock = "\u2590"
  )

  # attempt to make a bright alternative for scoped tags
  br_color <- if (is.character(color)) paste0("br_", color) else color
  tag_color <- if (is.null(scope)) color else br_color

  # convert colors to args
  color <- as.list(color)
  tag_color <- as.list(tag_color)

  # create styles for tag elements
  scope_fg <- do.call(cli::make_ansi_style, color)
  scope_bg <- do.call(cli::make_ansi_style, append(color, list(bg = TRUE)))
  tag_fg <- do.call(cli::make_ansi_style, tag_color)
  tag_bg <- do.call(cli::make_ansi_style, append(tag_color, list(bg = TRUE)))


  format_inline(
    .envir = .envir,
    scope_fg(symbols$lbracket),
    if (!is.null(scope)) {
      col_black(scope_bg(" ", scope, tag_fg(symbols$rhalfblock)))
    },
    style_bold(col_black(tag_bg(..., " "))),
    tag_fg(symbols$rbracket)
  )
}
