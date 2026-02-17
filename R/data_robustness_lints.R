#' @include impl_data.R

# Robustness lints metric implementation

#' Get list of robustness linters to apply
#'
#' Returns a list of lintr robustness linters that identify potentially
#' error-prone code patterns based on relatively unopinionated good programming
#' practices.
#'
#' @return List of linter functions
#' @noRd
get_robustness_linters <- function() {
  list(
    # Logic errors
    lintr::class_equals_linter(),
    lintr::equals_na_linter(),
    lintr::redundant_equals_linter(),
    lintr::vector_logic_linter(),
    lintr::outer_negation_linter(),
    
    # Iteration/sequence safety
    lintr::implicit_integer_linter(),
    lintr::seq_linter(),
    lintr::length_test_linter(),
    
    # String operations
    lintr::nzchar_linter(),
    lintr::paste_linter(),
    lintr::fixed_regex_linter(),
    
    # Conditionals
    lintr::redundant_ifelse_linter(),
    lintr::ifelse_censor_linter(),
    lintr::conjunct_test_linter(),
    
    # Functions/arguments
    lintr::duplicate_argument_linter(),
    lintr::missing_argument_linter(),
    lintr::condition_message_linter(),
    
    # Code organization
    lintr::unreachable_code_linter(),
    lintr::library_call_linter(),
    lintr::namespace_linter(),
    
    # Package development
    lintr::package_hooks_linter(),
    lintr::routine_registration_linter(),
    lintr::system_file_linter(),
    
    # Efficiency
    lintr::literal_coercion_linter(),
    lintr::scalar_in_linter(),
    
    # Best practices
    lintr::T_and_F_symbol_linter(),
    lintr::stopifnot_all_linter()
  )
}

# Auxiliary data: full lint results
impl_data(
  "robustness_lints",
  class = class_list,
  metric = FALSE,
  tags = character(0)
)

impl_data(
  "robustness_lints",
  for_resource = source_code_resource,
  function(pkg, resource, field, ...) {
    if (!requireNamespace("lintr", quietly = TRUE)) {
      return(list())
    }
    
    linters <- get_robustness_linters()
    
    # Lint the package source
    lints <- lintr::lint_package(
      path = resource@path,
      linters = linters,
      parse_settings = FALSE  # Don't read .lintr config for consistency
    )
    
    # Convert to list (lintr returns special S3 class)
    as.list(lints)
  }
)

impl_data(
  "robustness_lints",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    # Generate 0-10 mock lints (Poisson distribution, lambda = 3)
    n_lints <- rpois(1L, lambda = 3)
    
    if (n_lints == 0L) {
      return(list())
    }
    
    # Mock lint object structure
    replicate(n_lints, list(
      filename = "R/mock.R",
      line_number = sample(1L:100L, 1L),
      column_number = 1L,
      type = "warning",
      message = "Mock robustness lint",
      line = "# mock code",
      linter = "mock_linter"
    ), simplify = FALSE)
  }
)

# Primary metric: count of robustness lints
impl_data(
  "robustness_lint_count",
  metric = TRUE,
  class = class_integer,
  tags = tags("best practice"),
  suggests = suggests("lintr"),
  title = "Robustness Lint Count",
  description = paste(
    "Count of robustness lints identified by \\pkg{lintr}.",
    "Robustness lints identify potentially error-prone code patterns",
    "based on relatively unopinionated good programming practices.",
    "Lower counts indicate more robust code.",
    "See \\url{https://lintr.r-lib.org/reference/robustness_linters.html}"
  )
)

impl_data(
  "robustness_lint_count",
  for_resource = source_code_resource,
  function(pkg, resource, field, ...) {
    length(pkg$robustness_lints)
  }
)

impl_data(
  "robustness_lint_count",
  for_resource = mock_resource,
  function(pkg, resource, field, ...) {
    length(pkg$robustness_lints)
  }
)
