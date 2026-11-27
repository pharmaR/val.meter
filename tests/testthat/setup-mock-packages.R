#' Create a minimal package source directory for source_code_resource
create_mock_pkg_dir <- function(dir) {
  dir.create(file.path(dir, "R"), recursive = TRUE)
  writeLines(
    c(
      "Package: mockpkg",
      "Version: 1.0.0",
      "Title: Mock Package",
      "Description: A mock package for testing.",
      "Authors@R: person('A', 'B', role = 'aut')",
      "License: MIT + file LICENSE"
    ),
    file.path(dir, "DESCRIPTION")
  )
  writeLines("x <- 1", file.path(dir, "R", "mock.R"))
  invisible(dir)
}

#' Create a mocked source package from a minimal package source code directory
mock_source_pkg <- local({
  tmp <- file.path(tempdir(), paste0(packageName(), "-mocks"), "mockpkg")

  if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE, force = TRUE)
  }

  dir.create(tmp, recursive = TRUE)
  create_mock_pkg_dir(tmp)

  function(...) {
    pkg(source_code_resource(path = tmp, package = "mockpkg"), ...)
  }
})
