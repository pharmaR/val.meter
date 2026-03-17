#' @include impl_data.R

# Lint metric implementation

# Helper: count total non-empty R source lines in a package
count_r_loc <- function(path) {
  r_files <- list.files(
    file.path(path, "R"),
    pattern = "\\.R$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(r_files) == 0L) return(0L)
  total <- 0L
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    total <- total + sum(nzchar(trimws(lines)))
  }
  total
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
    "The set of linters applied is controlled by the \\code{lint_linters} option."
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
    "The set of linters applied is controlled by the \\code{lint_linters} option,",
    "which defaults to 26 linters from the \\emph{correctness},",
    "\\emph{common_mistakes}, and \\emph{robustness} tags.",
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
    "Fraction of non-empty R source lines that are free of lints,",
    "as identified by \\pkg{lintr}. A value of 1 means no linted lines;",
    "lower values indicate more pervasive lint issues.",
    "The set of linters applied is controlled by the \\code{lint_linters} option.",
    "Returns \\code{1} when the package has no R source lines."
  )
)

impl_data(
  "lint_free_fraction",
  for_resource = source_code_resource,
  function(pkg, resource, field, ...) {
    loc <- count_r_loc(resource@path)
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
