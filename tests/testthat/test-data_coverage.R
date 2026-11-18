test_that("coverage is calculated in a subprocess", {
  skip_on_cran()
  skip_if_not_installed("covr")
  skip_if_not_installed("callr")
  withr::local_options(val.meter.policy = policy(permissions = TRUE))

  p <- pkg(test_path("..", "fixtures", "pkg.covr.tests"))
  expect_s3_class(p$covr_coverage, "coverage")
  expect_identical(p$test_line_coverage_fraction, 0.5)
})

test_that("coverage with hanging tests is interrupted by the timeout", {
  skip_on_cran()
  skip_on_os("windows") # fixture hangs using `sleep`
  skip_if_not_installed("covr")
  skip_if_not_installed("callr")
  withr::local_options(
    val.meter.policy = policy(permissions = TRUE),
    val.meter.timeout = 10
  )
  withr::local_envvar(PKG_COVR_TESTS_HANG = "true")

  p <- pkg(test_path("..", "fixtures", "pkg.covr.tests"))
  elapsed <- system.time(value <- p$covr_coverage)[["elapsed"]]

  expect_s3_class(value, cnd_type("timeout")[[1L]])
  expect_lt(elapsed, 30)
})
