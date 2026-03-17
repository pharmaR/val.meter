# Formatted version of `capture.output()`

Captures output and formats it according to `style`. Uses the
[evaluate::evaluate](https://pharmar.github.io/val.meter/reference/evaluate.r-lib.org/reference/evaluate.md)
package to capture output to be more resilient to other processes
sinking output, causing issues with `capture.output` – notably when used
with [knitr::knitr](https://rdrr.io/pkg/knitr/man/knitr-package.html).

## Usage

``` r
format_output(
  x,
  ...,
  style = infer_format_style(),
  evaluate = TRUE,
  envir = parent.frame()
)
```

## Arguments

- x:

  An expression to capture output from.

- ...:

  Additional arguments unused.

- style:

  What style to format output as. When `knitr` is running, infers the
  preferred format from the output document type.

- evaluate:

  Whether to evaluate `x` by first capturing output with
  [evaluate::evaluate](https://pharmar.github.io/val.meter/reference/evaluate.r-lib.org/reference/evaluate.md).
  When `FALSE`, format `x` as a `character` value directly.

- envir:

  An environment in which expression `x` should be evaluated.

## Examples

``` r
format_output(
  cli::cli_text(cli::col_red("hello, world!")),
  type = "message",  # cli outputs to message stream
  style = "html"
)
#> <div>
#>   <style>
#> .r-cli--colors-8 .ansi-bold        { font-weight: bold;             }
#> .r-cli--colors-8 .ansi-italic      { font-style: italic;            }
#> .r-cli--colors-8 .ansi-underline   { text-decoration: underline;    }
#> .r-cli--colors-8 .ansi-blink       { text-decoration: blink;        }
#> .r-cli--colors-8 .ansi-hide        { visibility: hidden;            }
#> .r-cli--colors-8 .ansi-crossedout  { text-decoration: line-through; }
#> .r-cli--colors-8 .ansi-link:hover  { text-decoration: underline;    }
#> .r-cli--colors-8 .ansi-color-0     { color: #000000 }
#> .r-cli--colors-8 .ansi-color-1     { color: #cd3131 }
#> .r-cli--colors-8 .ansi-color-2     { color: #0dbc79 }
#> .r-cli--colors-8 .ansi-color-3     { color: #e5e510 }
#> .r-cli--colors-8 .ansi-color-4     { color: #2472c8 }
#> .r-cli--colors-8 .ansi-color-5     { color: #bc3fbc }
#> .r-cli--colors-8 .ansi-color-6     { color: #11a8cd }
#> .r-cli--colors-8 .ansi-color-7     { color: #e5e5e5 }
#> .r-cli--colors-8 .ansi-color-8     { color: #666666 }
#> .r-cli--colors-8 .ansi-color-9     { color: #f14c4c }
#> .r-cli--colors-8 .ansi-color-10    { color: #23d18b }
#> .r-cli--colors-8 .ansi-color-11    { color: #f5f543 }
#> .r-cli--colors-8 .ansi-color-12    { color: #3b8eea }
#> .r-cli--colors-8 .ansi-color-13    { color: #d670d6 }
#> .r-cli--colors-8 .ansi-color-14    { color: #29b8db }
#> .r-cli--colors-8 .ansi-color-15    { color: #e5e5e5 }
#> .r-cli--colors-8 .ansi-bg-color-0  { background-color: #000000 }
#> .r-cli--colors-8 .ansi-bg-color-1  { background-color: #cd3131 }
#> .r-cli--colors-8 .ansi-bg-color-2  { background-color: #0dbc79 }
#> .r-cli--colors-8 .ansi-bg-color-3  { background-color: #e5e510 }
#> .r-cli--colors-8 .ansi-bg-color-4  { background-color: #2472c8 }
#> .r-cli--colors-8 .ansi-bg-color-5  { background-color: #bc3fbc }
#> .r-cli--colors-8 .ansi-bg-color-6  { background-color: #11a8cd }
#> .r-cli--colors-8 .ansi-bg-color-7  { background-color: #e5e5e5 }
#> .r-cli--colors-8 .ansi-bg-color-8  { background-color: #666666 }
#> .r-cli--colors-8 .ansi-bg-color-9  { background-color: #f14c4c }
#> .r-cli--colors-8 .ansi-bg-color-10 { background-color: #23d18b }
#> .r-cli--colors-8 .ansi-bg-color-11 { background-color: #f5f543 }
#> .r-cli--colors-8 .ansi-bg-color-12 { background-color: #3b8eea }
#> .r-cli--colors-8 .ansi-bg-color-13 { background-color: #d670d6 }
#> .r-cli--colors-8 .ansi-bg-color-14 { background-color: #29b8db }
#> .r-cli--colors-8 .ansi-bg-color-15 { background-color: #e5e5e5 }
#> </style>
#>   <pre class="r-output r-cli--colors-8"><span class="ansi ansi-color-1">hello, world!</span></pre>
#> </div>
```
