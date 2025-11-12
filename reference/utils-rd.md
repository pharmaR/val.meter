# Rd utilities

These functions are used primarily for generating documentation
programmatically - most notably for documenting metrics that are already
documented using
[`pkg_data_info`](https://pharmar.github.io/val.meter/reference/pkg_data_info.md)
metadata objects.

## Usage

``` r
rd_empty()

rd_escape(text)

rd_to_txt(...)

rd_parse(text, fragment = FALSE, permissive = TRUE, ...)

rd_deparse(rd, deparse = TRUE, ...)

rd_sexpr(
  code,
  stage = c("build", "install", "render"),
  results = c("text", "verbatim", "rd", "hide"),
  quote = TRUE
)

rd_badge(
  message,
  label = "",
  style = "",
  dest = NULL,
  color = "blue",
  url = "https://img.shields.io/badge/",
  params = list(style = "flat-square")
)

rd_figure(filename, alt, options = list(alt = alt))

rd_ifelse(condition, true, false)

rd_href(content, dest)

rd_link(content, dest)
```

## Functions

- `rd_empty()`: Create an empty Rd object

- `rd_escape()`: Escape an Rd string

- `rd_to_txt()`: Convert Rd to text-formatted character vector

- `rd_parse()`: Parse text into an Rd format

- `rd_deparse()`: Deparse an Rd object into a character value

- `rd_sexpr()`: Convert R code to an Rd \Sexpr

- `rd_badge()`: Generate a badge, using shields.io and caching svg
  images for display in html output.

- `rd_figure()`: Generate `\figure{}` Rd output

- `rd_ifelse()`: Generate `\ifelse{}` Rd output

- `rd_href()`: Generate `\href{}` Rd output

- `rd_link()`: Generate `\link[]{}` Rd output
