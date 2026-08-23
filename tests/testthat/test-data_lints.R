# Helper: create a fake lintr `lints` object with n entries
create_mock_lints <- function(n = 3) {
  lint_list <- replicate(n, structure(
    list(
      filename = "R/test.R",
      line_number = 1L,
      column_number = 1L,
      type = "warning",
      message = "mock lint",
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

describe("lint_count derivation from mocked lints", {
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
        result <- p$lint_count
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
        result <- p$lint_count
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
        result <- p$lint_count
        expect_true(is.numeric(result))
        expect_true(result >= 0)
      }
    )
  })

  it("stores the full lints object in lints", {
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
        lints <- p$lints
        expect_true(inherits(lints, "lints"))
        expect_length(lints, 3)
      }
    )
  })
})

describe("lint_count mock implementation", {
  it("returns non-negative values", {
    results <- replicate(50, {
      p <- random_pkg()
      p$lint_count
    })

    expect_true(all(results >= 0))
  })

  it("returns numeric values consistently", {
    results <- replicate(20, {
      p <- random_pkg()
      p$lint_count
    })

    expect_true(all(vlapply(as.list(results), is.numeric)))
  })
})

describe("find_lintable_files helper", {
  it("returns empty character vector for package with no R files", {
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "R"))
    result <- find_lintable_files(tmp_dir)
    expect_type(result, "character")
    expect_length(result, 0L)
  })

  it("finds .R files in R/ directory", {
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "R"))
    writeLines("x <- 1", file.path(tmp_dir, "R", "a.R"))
    writeLines("y <- 2", file.path(tmp_dir, "R", "b.R"))
    result <- find_lintable_files(tmp_dir)
    expect_length(result, 2L)
    expect_true(all(grepl("\\.R$", result)))
  })

  it("finds files in tests/ directory", {
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "tests", "testthat"), recursive = TRUE)
    writeLines(
      "test_that('x', {})",
      file.path(tmp_dir, "tests", "testthat", "test-a.R")
    )
    result <- find_lintable_files(tmp_dir)
    expect_length(result, 1L)
  })

  it("finds files across multiple package directories", {
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "R"))
    dir.create(file.path(tmp_dir, "tests"))
    dir.create(file.path(tmp_dir, "vignettes"))
    writeLines("x <- 1", file.path(tmp_dir, "R", "code.R"))
    writeLines("test_that('x', {})", file.path(tmp_dir, "tests", "test.R"))
    writeLines("# vignette", file.path(tmp_dir, "vignettes", "intro.R"))
    result <- find_lintable_files(tmp_dir)
    expect_length(result, 3L)
  })

  it("respects custom pkg_dirs parameter", {
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "R"))
    dir.create(file.path(tmp_dir, "tests"))
    writeLines("x <- 1", file.path(tmp_dir, "R", "code.R"))
    writeLines("test_that('x', {})", file.path(tmp_dir, "tests", "test.R"))
    result <- find_lintable_files(tmp_dir, pkg_dirs = "R")
    expect_length(result, 1L)
    expect_true(grepl("/R/", result))
  })

  it("respects custom file_pattern parameter", {
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "R"))
    writeLines("x <- 1", file.path(tmp_dir, "R", "code.R"))
    writeLines("---\ntitle: test\n---", file.path(tmp_dir, "R", "doc.Rmd"))
    # Only .R files
    result <- find_lintable_files(tmp_dir, file_pattern = "\\.R$")
    expect_length(result, 1L)
    expect_true(grepl("\\.R$", result))
  })

  it("ignores non-existent directories", {
    tmp_dir <- withr::local_tempdir()
    # No directories created
    result <- find_lintable_files(tmp_dir)
    expect_type(result, "character")
    expect_length(result, 0L)
  })
})

describe("count_file_code_lines helper", {
  it("counts code lines in a simple R file", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    file_path <- file.path(tmp_dir, "test.R")
    writeLines(c("x <- 1", "y <- 2", "z <- 3"), file_path)
    result <- count_file_code_lines(file_path)
    expect_equal(result, 3L)
  })

  it("excludes blank lines", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    file_path <- file.path(tmp_dir, "test.R")
    writeLines(c("x <- 1", "", "", "y <- 2"), file_path)
    result <- count_file_code_lines(file_path)
    expect_equal(result, 2L)
  })

  it("excludes comment-only lines", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    file_path <- file.path(tmp_dir, "test.R")
    writeLines(c("# comment", "x <- 1", "#' roxygen", "y <- 2"), file_path)
    result <- count_file_code_lines(file_path)
    expect_equal(result, 2L)
  })

  it("returns 0L for empty file", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    file_path <- file.path(tmp_dir, "test.R")
    writeLines(character(0), file_path)
    result <- count_file_code_lines(file_path)
    expect_equal(result, 0L)
  })

  it("returns 0L for comment-only file", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    file_path <- file.path(tmp_dir, "test.R")
    writeLines(c("# just", "# comments"), file_path)
    result <- count_file_code_lines(file_path)
    expect_equal(result, 0L)
  })
})

describe("count_lintable_code_lines helper", {
  it("returns 0 for a package with no R files", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "R"))
    result <- count_lintable_code_lines(tmp_dir)
    expect_equal(result, 0L)
  })

  it("counts token-bearing non-comment lines across R files", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "R"))
    # Line 1: code, Line 2: blank (excluded), Line 3: code
    writeLines(c("x <- 1", "", "y <- 2"), file.path(tmp_dir, "R", "a.R"))
    # Line 1: code
    writeLines(c("z <- 3"), file.path(tmp_dir, "R", "b.R"))
    result <- count_lintable_code_lines(tmp_dir)
    expect_equal(result, 3L)
  })

  it("excludes comment-only lines", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "R"))
    # 2 code lines, 2 comment lines, 1 blank
    writeLines(
      c("# This is a comment", "x <- 1", "", "#' roxygen", "y <- 2"),
      file.path(tmp_dir, "R", "a.R")
    )
    result <- count_lintable_code_lines(tmp_dir)
    expect_equal(result, 2L)
  })

  it("includes tests/ directory in scope", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "R"))
    dir.create(file.path(tmp_dir, "tests", "testthat"), recursive = TRUE)
    writeLines("x <- 1", file.path(tmp_dir, "R", "a.R"))
    writeLines("test_that('works', { expect_true(TRUE) })",
               file.path(tmp_dir, "tests", "testthat", "test-a.R"))
    result <- count_lintable_code_lines(tmp_dir)
    # 1 from R/, 1 from tests/
    expect_equal(result, 2L)
  })
})

describe("count_linted_lines helper", {
  it("returns 0 for empty lints", {
    result <- count_linted_lines(create_mock_lints(0))
    expect_equal(result, 0L)
  })

  it("deduplicates lines with multiple lints", {
    lints_same_line <- structure(
      list(
        structure(
          list(filename = "R/a.R", line_number = 5L, column_number = 1L,
               type = "warning", message = "lint 1", line = "x", ranges = NULL),
          class = "lint"
        ),
        structure(
          list(filename = "R/a.R", line_number = 5L, column_number = 3L,
               type = "warning", message = "lint 2", line = "x", ranges = NULL),
          class = "lint"
        ),
        structure(
          list(filename = "R/a.R", line_number = 6L, column_number = 1L,
               type = "warning", message = "lint 3", line = "y", ranges = NULL),
          class = "lint"
        )
      ),
      class = c("lints", "list")
    )
    result <- count_linted_lines(lints_same_line)
    expect_equal(result, 2L)
  })
})

describe("lint_free_fraction derivation", {
  it("returns 1 when there are no lints", {
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
        result <- p$lint_free_fraction
        expect_equal(result, 1.0)
      }
    )
  })

  it("returns a value in [0, 1]", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    create_mock_pkg_dir(tmp_dir)

    with_mocked_bindings(
      lint_package = function(...) create_mock_lints(2),
      .package = "lintr",
      {
        p <- pkg(
          source_code_resource(path = tmp_dir, package = "mockpkg"),
          permissions = permissions("execution")
        )
        result <- p$lint_free_fraction
        expect_true(is.numeric(result))
        expect_true(result >= 0.0 && result <= 1.0)
      }
    )
  })
})

describe("lint_free_fraction mock implementation", {
  it("returns values in [0, 1]", {
    results <- replicate(50, {
      p <- random_pkg()
      p$lint_free_fraction
    })

    expect_true(all(results >= 0.0 & results <= 1.0))
  })

  it("returns numeric values consistently", {
    results <- replicate(20, {
      p <- random_pkg()
      p$lint_free_fraction
    })

    expect_true(all(vlapply(as.list(results), is.numeric)))
  })
})

describe("lint_linters option customization", {
  it("uses the linter names from the option", {
    skip_if_not_installed("lintr")
    tmp_dir <- withr::local_tempdir()
    create_mock_pkg_dir(tmp_dir)

    called_with <- NULL
    old <- opt_set("lint_linters", c("equals_na_linter", "seq_linter"))
    on.exit(opt_set("lint_linters", old))

    with_mocked_bindings(
      lint_package = function(..., linters) {
        called_with <<- names(linters)
        create_mock_lints(0)
      },
      .package = "lintr",
      {
        p <- pkg(
          source_code_resource(path = tmp_dir, package = "mockpkg"),
          permissions = permissions("execution")
        )
        p$lints
      }
    )

    expect_equal(sort(called_with), c("equals_na_linter", "seq_linter"))
  })

  it("can restrict to a subset of linters", {
    old <- opt_set("lint_linters", c("equals_na_linter"))
    on.exit(opt_set("lint_linters", old))

    linters <- opt("lint_linters")
    expect_equal(linters, "equals_na_linter")
    expect_length(linters, 1L)
  })
})
