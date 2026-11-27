checks <- data.frame(
  Flavor = rep(c("a", "b", "c"), times = 2L),
  Package = rep(c("pkg1", "pkg2"), each = 3L),
  Version = rep(c("1.0.0", "2.0.0"), each = 3L),
  Status = factor(
    c("OK", "OK", "OK", "NOTE", "WARNING", "ERROR"),
    levels = c("OK", "NOTE", "WARNING", "ERROR", "FAILURE")
  )
)

with_mocked_cran_checks <- function(...) {
  testthat::with_mocked_bindings(
    cran_check_results = function(...) checks,
    ...
  )
}

with_mocked_cran_checks(describe("pkg$cran_statuses", {
  it("filters for package version match by default", {
    p <- pkg(
      mock_resource(package = "pkg1", version = "1.0.0"),
      permissions = TRUE
    )

    expect_length(p$cran_status, 3L)
    expect_true(all(p$cran_status == "OK"))

    p <- pkg(
      mock_resource(package = "pkg1", version = "0.9.0"),
      permissions = TRUE
    )

    expect_length(p$cran_status, 0L)
  })

  it("provides current checks regardless of version when option is enabled", {
    withr::with_options(
      list(val.meter.metrics.cran_status.ignore_version_mismatch = TRUE),
      {
        p <- pkg(
          mock_resource(package = "pkg1", version = "0.9.9"),
          permissions = TRUE
        )

        expect_length(p$cran_status, 3L)
        expect_true(all(p$cran_status == "OK"))
      }
    )
  })
}))

with_mocked_cran_checks(describe("pkg$cran_status_*_fraction", {
  it("provides fractions of associated status", {
    p <- pkg(
      mock_resource(package = "pkg2", version = "2.0.0"),
      permissions = TRUE
    )

    expect_length(p$cran_status, 3L)
    expect_true(all(p$cran_status != "OK"))
    expect_true(p$cran_status_note_fraction == 1 / 3)
    expect_true(p$cran_status_warning_fraction == 1 / 3)
    expect_true(p$cran_status_error_fraction == 1 / 3)
  })
}))
