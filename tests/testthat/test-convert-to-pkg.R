test_that("convert from character to pkg can discover resources", {
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
