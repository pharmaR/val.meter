#' @include impl_data.R

# Lint metric implementation

#' Find lintable files in a package
#'
#' @param path Package source directory.
#' @param pkg_dirs Subdirectories to search.
#' @param file_pattern Regex for file extensions.
#' @return Character vector of file paths.
#' @noRd
find_lintable_files <- function(
    path,
    pkg_dirs = c("R", "tests", "inst", "vignettes", "data-raw", "demo", "exec"),
    file_pattern = "(?i)[.](r|rmd|qmd|rnw|rhtml|rrst|rtex|rtxt)$"
) {
  r_files <- character(0)
  for (dir in pkg_dirs) {
    dir_path <- file.path(path, dir)
    if (dir.exists(dir_path)) {
      files <- list.files(
        dir_path,
        pattern = file_pattern,
        recursive = TRUE,
        full.names = TRUE
      )
      r_files <- c(r_files, files)
    }
  }
  r_files
}

#' Count token-bearing, non-comment lines in a single file
#'
#' @param file Path to an R or Rmd file.
#' @return Integer count of code lines, or 0L on parse error.
#' @noRd
count_file_code_lines <- function(file) {
  tryCatch(
    {
      se <- lintr::get_source_expressions(file)
      full_expr <- se$expressions[[length(se$expressions)]]
      pd <- full_expr$full_parsed_content

      if (!is.null(pd) && nrow(pd) > 0L) {
        length(unique(pd$line1[pd$terminal & pd$token != "COMMENT"]))
      } else {
        0L
      }
    },
    error = function(e) 0L
  )
}

#' Count token-bearing, non-comment lines in files that lintr processes
#'
#' Uses lintr's own parsing frontend to ensure the denominator matches
#' the scope of lint_package(). A "relevant code line" is a physical
#' line containing at least one terminal token that is not a comment.
#'
#' @param path Path to package source directory.
#' @param pkg_dirs Character vector of subdirectories to search.
#' @param file_pattern Regex pattern for file extensions.
#' @return Integer count of code lines.
#' @noRd
count_lintable_code_lines <- function(
    path,
    pkg_dirs = c("R", "tests", "inst", "vignettes", "data-raw", "demo", "exec"),
    file_pattern = "(?i)[.](r|rmd|qmd|rnw|rhtml|rrst|rtex|rtxt)$"
) {
  r_files <- find_lintable_files(path, pkg_dirs, file_pattern)

  if (length(r_files) == 0L) {
    return(0L)
  }

  total_lines <- 0L
  for (file in r_files) {
    total_lines <- total_lines + count_file_code_lines(file)
  }

  total_lines
}

# Helper: count unique (filename, line_number) pairs across all lints
count_linted_lines <- function(lints) {
  if (length(lints) == 0L) return(0L)
  pairs <- vcapply(lints, function(l) paste0(l$filename, ":", l$line_number))
  length(unique(pairs))
}

# Auxiliary data: full lint results
impl_data(
  "lints",
  class = S7::new_S3_class("lints"),
  metric = FALSE,
  tags = c("execution"),
  suggests = "lintr",
  permissions = "execution",
  title = "Lint Results",
  description = paste(
    "Full \\pkg{lintr} lint results for the package.",
    "Contains detailed information about each lint including line numbers,",
    "column positions, and lint messages. This is auxiliary data used to",
    "derive the \\code{lint_count} and \\code{lint_free_fraction} metrics.",
    "The set of linters applied is controlled by the \\code{lint_linters}",
    "option."
  )
)

impl_data(
  "lints",
  for_resource = source_code_resource,
  function(pkg, resource, field, ..., quiet = opt("quiet")) {
    linter_names <- opt("lint_linters")
    linters <- setNames(
      lapply(linter_names, function(nm) utils::getFromNamespace(nm, "lintr")()),
      linter_names
    )
    wrapper <- if (quiet) {
      function(...) capture.output(..., type = "message")
    } else {
      identity
    }

    wrapper({
      result <- lintr::lint_package(
        path = resource@path,
        linters = linters,
        parse_settings = FALSE # Don't read .lintr config for consistency
      )
    })

    result
  }
)

# Primary metric: count of lints
impl_data(
  "lint_count",
  metric = TRUE,
  class = class_integer,
  tags = c("best practice"),
  title = "Lint Count",
  description = paste(
    "Count of lints identified by \\pkg{lintr}.",
    "The set of linters applied is controlled by the \\code{lint_linters}",
    "option, which defaults to linters from the \\emph{correctness},",
    "\\emph{common_mistakes}, and \\emph{robustness} tags excluding",
    "those that require configuration or are environment-dependent.",
    "Lower counts indicate higher code quality."
  )
)

impl_data(
  "lint_count",
  for_resource = source_code_resource,
  function(pkg, resource, field, ...) {
    length(pkg$lints)
  }
)

impl_data(
  "lint_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) rpois(1, 10)
)

# Secondary metric: fraction of lintable lines that are lint-free
impl_data(
  "lint_free_fraction",
  metric = TRUE,
  class = class_double,
  tags = c("best practice"),
  title = "Lint-Free Fraction",
  description = paste(
    "Fraction of R code lines that are free of lints.",
    "Code lines are defined as lines containing executable R tokens",
    "(excluding comments and blank lines) across the standard package",
    "directories (\\code{R/}, \\code{tests/}, \\code{inst/},",
    "\\code{vignettes/}, \\code{data-raw/}, \\code{demo/}, \\code{exec/}).",
    "A value of 1 means no linted lines; lower values indicate more",
    "pervasive lint issues. The set of linters applied is controlled by",
    "the \\code{lint_linters} option."
  )
)

impl_data(
  "lint_free_fraction",
  for_resource = source_code_resource,
  function(pkg, resource, field, ...) {
    loc <- count_lintable_code_lines(resource@path)
    if (loc == 0L) return(1.0)
    linted <- count_linted_lines(pkg$lints)
    max(0.0, 1.0 - linted / loc)
  }
)

impl_data(
  "lint_free_fraction",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    runif(1, min = 0.7, max = 1.0)
  }
)
