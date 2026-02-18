describe("reverse dependencies helper functions", {
  it("fetch_packages_matrix returns a matrix", {
    skip_if_offline()
    skip_on_cran()

    matrix <- fetch_packages_matrix(repos = "https://cloud.r-project.org")

    expect_true(is.matrix(matrix))
    expect_true(nrow(matrix) > 0)
    expect_true("Package" %in% colnames(matrix))
  })

  it("get_reverse_deps returns character vector", {
    skip_if_offline()
    skip_on_cran()

    matrix <- fetch_packages_matrix(repos = "https://cloud.r-project.org")

    # Use a popular package that's likely to have reverse dependencies
    result <- get_reverse_deps("testthat", matrix)

    expect_type(result, "character")
  })

  it("get_reverse_deps filters by dependency types", {
    skip_if_offline()
    skip_on_cran()

    matrix <- fetch_packages_matrix(repos = "https://cloud.r-project.org")

    # Test with different dependency types
    deps_all <- get_reverse_deps(
      "testthat",
      matrix,
      dependencies = c("Depends", "Imports", "LinkingTo", "Suggests")
    )

    deps_strong <- get_reverse_deps(
      "testthat",
      matrix,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )

    # Strong dependencies should be a subset of all dependencies
    expect_true(all(deps_strong %in% deps_all))
  })
})

describe("cran_reverse_dependencies metadata", {
  it("is registered as non-metric data", {
    info <- pkg_data_info("cran_reverse_dependencies")
    expect_false(info@metric)
  })

  it("has 'adoption' and 'transient' tags", {
    info <- pkg_data_info("cran_reverse_dependencies")
    expect_true("adoption" %in% info@tags)
    expect_true("transient" %in% info@tags)
  })

  it("requires 'network' permission", {
    info <- pkg_data_info("cran_reverse_dependencies")
    expect_true("network" %in% info@permissions)
  })

  it("has a non-empty title", {
    info <- pkg_data_info("cran_reverse_dependencies")
    expect_true(length(info@title) > 0)
    expect_true(nchar(info@title) > 0)
  })

  it("has a non-empty description", {
    info <- pkg_data_info("cran_reverse_dependencies")
    expect_true(length(info@description) > 0)
  })

  it("has class_character data class", {
    info <- pkg_data_info("cran_reverse_dependencies")
    expect_equal(info@data_class, class_character)
  })
})

describe("cran_reverse_dependencies_count metadata", {
  it("is registered as a metric", {
    info <- pkg_data_info("cran_reverse_dependencies_count")
    expect_true(info@metric)
  })

  it("has 'adoption' and 'transient' tags", {
    info <- pkg_data_info("cran_reverse_dependencies_count")
    expect_true("adoption" %in% info@tags)
    expect_true("transient" %in% info@tags)
  })

  it("does not require permissions", {
    info <- pkg_data_info("cran_reverse_dependencies_count")
    expect_identical(info@permissions, permissions(character(0L)))
  })

  it("has a non-empty title", {
    info <- pkg_data_info("cran_reverse_dependencies_count")
    expect_true(length(info@title) > 0)
    expect_true(nchar(info@title) > 0)
  })

  it("has a non-empty description", {
    info <- pkg_data_info("cran_reverse_dependencies_count")
    expect_true(length(info@description) > 0)
  })

  it("has class_integer data class", {
    info <- pkg_data_info("cran_reverse_dependencies_count")
    expect_equal(info@data_class, class_integer)
  })
})

describe("cran_reverse_dependencies mock implementation", {
  it("returns character vector for mock resource", {
    p <- random_pkg()
    result <- p$cran_reverse_dependencies

    expect_type(result, "character")
  })

  it("generates realistic mock data", {
    # Generate multiple random packages and check distribution
    n <- 50
    results <- replicate(n, {
      p <- random_pkg()
      length(p$cran_reverse_dependencies)
    })

    # Should be integers
    expect_type(results, "integer")

    # Should have reasonable range (0-10 based on mock implementation)
    expect_true(all(results >= 0))
    expect_true(all(results <= 10))
  })
})

describe("cran_reverse_dependencies_count mock implementation", {
  it("returns integer count for mock resource", {
    p <- random_pkg()
    result <- p$cran_reverse_dependencies_count

    expect_type(result, "integer")
    expect_length(result, 1)
  })

  it("count matches length of reverse dependencies", {
    p <- random_pkg()

    count <- p$cran_reverse_dependencies_count
    deps <- p$cran_reverse_dependencies

    expect_equal(count, length(deps))
  })
})

describe("cran_reverse_dependencies integration", {
  it("works with CRAN repository resource", {
    skip_if_offline()
    skip_on_cran()

    # Use a well-known package with a proper CRAN mirror (note trailing slash)
    p <- pkg(
      cran_repo_resource(
        package = "jsonlite",
        repo = "https://cloud.r-project.org/"
      ),
      permissions = permissions("network")
    )

    deps <- p$cran_reverse_dependencies

    expect_type(deps, "character")
    # jsonlite is popular, should have some reverse dependencies
    expect_true(length(deps) > 0)
  })

  it("count matches length for CRAN packages", {
    skip_if_offline()
    skip_on_cran()

    p <- pkg(
      cran_repo_resource(
        package = "jsonlite",
        repo = "https://cloud.r-project.org/"
      ),
      permissions = permissions("network")
    )

    count <- p$cran_reverse_dependencies_count
    deps <- p$cran_reverse_dependencies

    expect_equal(count, length(deps))
    expect_type(count, "integer")
  })
})

describe("reverse dependencies type stability", {
  it("cran_reverse_dependencies returns character vector (not NA)", {
    # Even with no matches, should return character(0), not NA_character_
    p <- random_pkg()
    result <- p$cran_reverse_dependencies

    expect_type(result, "character")
    expect_false(anyNA(result))
  })

  it("cran_reverse_dependencies_count is always scalar integer", {
    results <- replicate(20, {
      p <- random_pkg()
      p$cran_reverse_dependencies_count
    })

    expect_type(results, "integer")
    expect_equal(length(results), 20)
  })
})
