# Helper: create a fake lintr `lints` object with n entries
create_mock_lints <- function(n = 3) {
  lint_list <- replicate(n, structure(
    list(
      filename = "R/test.R",
      line_number = 1L,
      column_number = 1L,
      type = "warning",
      message = "mock robustness lint",
      line = "x = 1",
      ranges = NULL
    ),
    class = "lint"
  ), simplify = FALSE)

  structure(lint_list, class = c("lints", "list"))
}

# Helper: create a minimal package source directory for source_code_resource
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

describe("robustness_lint_count derivation from mocked lints", {
  it("returns 0 when there are no lints", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    create_mock_pkg_dir(tmp_dir)

    with_mocked_bindings(
      lint_package = function(...) create_mock_lints(0),
      .package = "lintr",
      {
        p <- pkg(
          source_code_resource(path = tmp_dir, package = "mockpkg"),
          permissions = permissions("execution")
        )
        result <- p$robustness_lint_count
        expect_equal(result, 0L)
      }
    )
  })

  it("returns the correct count matching number of lints", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    create_mock_pkg_dir(tmp_dir)

    with_mocked_bindings(
      lint_package = function(...) create_mock_lints(7),
      .package = "lintr",
      {
        p <- pkg(
          source_code_resource(path = tmp_dir, package = "mockpkg"),
          permissions = permissions("execution")
        )
        result <- p$robustness_lint_count
        expect_equal(result, 7L)
      }
    )
  })

  it("returns a non-negative integer", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    create_mock_pkg_dir(tmp_dir)

    with_mocked_bindings(
      lint_package = function(...) create_mock_lints(4),
      .package = "lintr",
      {
        p <- pkg(
          source_code_resource(path = tmp_dir, package = "mockpkg"),
          permissions = permissions("execution")
        )
        result <- p$robustness_lint_count
        expect_true(is.numeric(result))
        expect_true(result >= 0)
      }
    )
  })

  it("stores the full lints object in robustness_lints", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    create_mock_pkg_dir(tmp_dir)

    with_mocked_bindings(
      lint_package = function(...) create_mock_lints(3),
      .package = "lintr",
      {
        p <- pkg(
          source_code_resource(path = tmp_dir, package = "mockpkg"),
          permissions = permissions("execution")
        )
        lints <- p$robustness_lints
        expect_true(inherits(lints, "lints"))
        expect_length(lints, 3)
      }
    )
  })
})

describe("robustness_lint_count mock implementation", {
  it("returns non-negative values", {
    results <- replicate(50, {
      p <- random_pkg()
      p$robustness_lint_count
    })

    expect_true(all(results >= 0))
  })

  it("returns numeric values consistently", {
    results <- replicate(20, {
      p <- random_pkg()
      p$robustness_lint_count
    })

    expect_true(all(vlapply(as.list(results), is.numeric)))
  })
})
