# Helper to create a mock BioConductor package matrix for testing
create_mock_bioc_matrix <- function() {
  matrix(
    c(
      # Package, Version, Priority, Depends, Imports, LinkingTo, Suggests
      "biocA", "1.0.0", NA, "R (>= 3.5.0), targetpkg", NA, NA, NA,
      "biocB", "2.0.0", NA, "R (>= 4.0.0)", "targetpkg, utils", NA, NA,
      "biocC", "1.5.0", NA, NA, NA, NA, "targetpkg, testthat",
      "biocD", "3.0.0", NA, NA, "targetpkg", NA, NA,
      "biocE", "1.2.0", NA, NA, "otherpkg", NA, NA
    ),
    nrow = 5,
    ncol = 7,
    byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4", "5"),
      c(
        "Package", "Version", "Priority", "Depends",
        "Imports", "LinkingTo", "Suggests"
      )
    )
  )
}

describe("get_bioc_repos", {
  it("returns the standard BioConductor repository URLs", {
    repos <- get_bioc_repos()

    expect_type(repos, "character")
    expect_true(length(repos) >= 1)
    expect_true(all(grepl("^https://bioconductor.org/packages/", repos)))
    expect_true(any(grepl("/bioc$", repos)))
  })

  it("targets a specific release when asked", {
    expect_true(all(grepl("/3.18/", get_bioc_repos(version = "3.18"))))
  })

  it("honours the val.meter.bioc_repos option override", {
    withr::with_options(
      list(val.meter.bioc_repos = "file:///tmp/fake-bioc"),
      expect_equal(get_bioc_repos(), "file:///tmp/fake-bioc")
    )
  })
})

describe("bioc_reverse_dependencies metric behavior with mocked data", {
  it("returns dependent package names when the BioC universe is mocked", {
    mock_matrix <- create_mock_bioc_matrix()

    with_mocked_bindings(
      viable_revdep_packages = function(repos) mock_matrix,
      .package = "val.meter",
      {
        p <- pkg(
          cran_repo_resource(
            package = "targetpkg",
            repo = "https://cloud.r-project.org/"
          ),
          permissions = permissions("network")
        )

        deps <- p$bioc_reverse_dependencies

        expect_type(deps, "character")
        # strong deps only (Depends/Imports/LinkingTo), not Suggests
        expect_true(all(c("biocA", "biocB", "biocD") %in% deps))
        expect_false("biocC" %in% deps)
      }
    )
  })

  it("returns an empty character vector when no reverse deps exist", {
    mock_matrix <- create_mock_bioc_matrix()

    with_mocked_bindings(
      viable_revdep_packages = function(repos) mock_matrix,
      .package = "val.meter",
      {
        p <- pkg(
          cran_repo_resource(
            package = "lonelypkg",
            repo = "https://cloud.r-project.org/"
          ),
          permissions = permissions("network")
        )

        deps <- p$bioc_reverse_dependencies

        expect_type(deps, "character")
        expect_length(deps, 0)
      }
    )
  })

  it("generates realistic mock data for a mock resource (offline)", {
    p <- pkg(
      mock_resource(package = "fakepkg", version = "1.2.3"),
      permissions("network")
    )

    deps <- p$bioc_reverse_dependencies

    expect_type(deps, "character")
    expect_true(length(deps) >= 0 && length(deps) <= 10)
  })
})

describe("bioc_reverse_dependencies_count metric behavior", {
  it("returns integer count matching dependency vector length", {
    mock_matrix <- create_mock_bioc_matrix()

    with_mocked_bindings(
      viable_revdep_packages = function(repos) mock_matrix,
      .package = "val.meter",
      {
        p <- pkg(
          cran_repo_resource(
            package = "targetpkg",
            repo = "https://cloud.r-project.org/"
          ),
          permissions = permissions("network")
        )

        count <- p$bioc_reverse_dependencies_count
        deps <- p$bioc_reverse_dependencies

        expect_type(count, "integer")
        expect_length(count, 1)
        expect_equal(count, length(deps))
      }
    )
  })

  it("is registered as a metric", {
    expect_true("bioc_reverse_dependencies_count" %in% names(metrics()))
  })
})
