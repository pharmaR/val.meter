test_that("convert(from = character, to = class_pkg) can discover resources", {
  expected <- simpleError("downloading ...")
  class(expected) <- c("test_suite_signal", class(expected))

  # intercept download package call and instead of doing a slow download, just
  # signal that we hit our download call
  with_mocked_bindings(
    available.packages = function(...) {
      cbind(
        Package = "fake.pkg",
        Version = "1.0",
        MD5sum = "abcdef",
        Repository = "acme.org"
      )
    },
    download.packages = function(...) {
      signalCondition(expected)
    },
    code = {
      # specify a policy that will force pkg to attempt re-download
      policy <- policy(
        accepted_resources = list(class_source_archive_resource),
        source_resources = list(class_repo_resource),
        permissions = TRUE
      )

      expect_error(
        pkg("fake.pkg", policy = policy),
        class = class(expected)[[1L]]
      )
    }
  )
})

test_that("convert(from = class_pkg, to = class_pkg)", {
  p <- random_pkg()
  expect_identical(p, convert(p, class_pkg))
})

test_that("convert(from = class_list, to = class_pkg)", {
  pkg_data <- list(
    name = "test",
    version = "1.2.3",
    r_cmd_check_error_count = 3L
  )

  expect_no_error(p <- convert(pkg_data, class_pkg))
  expect_identical(pkg_data$name, p$name)
  expect_identical(pkg_data$version, p$version)
  expect_identical(
    pkg_data$r_cmd_check_error_count,
    p$r_cmd_check_error_count
  )
})

test_that("convert(from = class_character [DCF], to = class_pkg)", {
  desc <- "
Package: test
Version: 1.2.3
Metric/r_cmd_check_error_count@R: 3L
  "

  expect_no_error(p <- convert(desc, class_pkg))
  expect_identical(p$name, "test")
  expect_identical(p$version, "1.2.3")
  expect_identical(p$r_cmd_check_error_count, 3L)
})

test_that("convert(from = class_character [*.Rds], to = class_pkg)", {
  orig_p <- random_pkg()
  f <- tempfile("test-pkg-", fileext = ".Rds")
  on.exit(file.remove(f))
  saveRDS(orig_p, f)

  expect_no_error(p <- convert(f, class_pkg))
  expect_identical(p$name, orig_p$name)
  expect_identical(p$version, orig_p$version)
})
