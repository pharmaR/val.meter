describe("to_dcf, pkgs_from_dcf", {
  it("Can round-trip packages to and from a DCF-formatted string", {
    packages <- random_pkgs(n = 3, permissions = TRUE)
    expect_no_error(str_dcf <- to_dcf(packages))
    expect_no_error(packages2 <- pkgs_from_dcf(str_dcf))
    expect_equal(
      packages[[1]]$help_examples_coverage,
      packages2[[1]]$help_examples_coverage
    )
  })

  it("Can round-trip errors produced by val.meter", {
    package <- mock_source_pkg()
    package$downloads_total

    packages <- list_of_pkg(list(package))

    expect_no_error(str_dcf <- to_dcf(packages))
    expect_no_error(packages2 <- pkgs_from_dcf(str_dcf))
    expect_equal(
      packages[[1]]$dependency_count,
      packages2[[1]]$dependency_count
    )

    expect_s3_class(
      packages2[[1]]$downloads_total,
      cnd_type("disallowed_permissions")
    )

    expect_identical(
      packages[[1]]$downloads_total$message,
      packages2[[1]]$downloads_total$message
    )
  })
})
