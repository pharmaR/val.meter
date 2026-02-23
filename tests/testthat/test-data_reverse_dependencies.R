# Helper to create a mock package matrix for testing
create_mock_package_matrix <- function() {
  # Create a minimal matrix that matches available.packages() structure
  # with the key columns needed by dependsOnPkgs()
  matrix(
    c(
      # Package, Version, Priority, Depends, Imports, LinkingTo, Suggests
      "pkgA", "1.0.0", NA, "R (>= 3.5.0), targetpkg", NA, NA, NA,
      "pkgB", "2.0.0", NA, "R (>= 4.0.0)", "targetpkg, utils", NA, NA,
      "pkgC", "1.5.0", NA, NA, NA, NA, "targetpkg, testthat",
      "pkgD", "3.0.0", NA, NA, "targetpkg", NA, NA,
      "pkgE", "1.2.0", NA, NA, "otherpkg", NA, NA
    ),
    nrow = 5,
    ncol = 7,
    byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4", "5"),
      c("Package", "Version", "Priority", "Depends", "Imports", "LinkingTo", "Suggests")
    )
  )
}

describe("reverse dependencies helper functions", {
  it("get_reverse_deps returns character vector of dependent packages", {
    mock_matrix <- create_mock_package_matrix()
    
    result <- get_reverse_deps("targetpkg", mock_matrix)
    
    expect_type(result, "character")
    # Should find pkgA, pkgB, pkgD (in Depends/Imports, not Suggests)
    expect_true(length(result) >= 3)
    expect_true("pkgA" %in% result)
    expect_true("pkgB" %in% result)
    expect_true("pkgD" %in% result)
  })
  
  it("get_reverse_deps filters by dependency types correctly", {
    mock_matrix <- create_mock_package_matrix()
    
    # All dependency types including Suggests
    deps_all <- get_reverse_deps(
      "targetpkg", 
      mock_matrix,
      dependencies = c("Depends", "Imports", "LinkingTo", "Suggests")
    )
    
    # Only strong dependencies (excludes Suggests)
    deps_strong <- get_reverse_deps(
      "targetpkg", 
      mock_matrix,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
    
    # Strong dependencies should be a subset of all dependencies
    expect_true(all(deps_strong %in% deps_all))
    # All should include pkgC (from Suggests), so it should have more
    expect_true(length(deps_all) > length(deps_strong))
    expect_true("pkgC" %in% deps_all)
    expect_false("pkgC" %in% deps_strong)
  })
  
  it("get_reverse_deps returns empty vector when no dependencies found", {
    mock_matrix <- create_mock_package_matrix()
    
    result <- get_reverse_deps("nonexistent", mock_matrix)
    
    expect_type(result, "character")
    expect_length(result, 0)
  })
})

describe("cran_reverse_dependencies metric behavior with mocked data", {
  it("returns character vector when get_available_packages is mocked", {
    mock_matrix <- create_mock_package_matrix()
    
    with_mocked_bindings(
      get_available_packages = function(repos) mock_matrix,
      .package = "val.meter",
      {
        p <- pkg(
          cran_repo_resource(
            package = "targetpkg",
            repo = "https://cloud.r-project.org/"
          ),
          permissions = permissions("network")
        )
        
        deps <- p$cran_reverse_dependencies
        
        expect_type(deps, "character")
        expect_true(length(deps) > 0)
      }
    )
  })
  
  it("returns empty character vector when no reverse deps exist", {
    # Create a matrix where no packages depend on our target
    mock_matrix <- matrix(
      c(
        "pkgA", "1.0.0", NA, NA, NA, NA, NA,
        "pkgB", "2.0.0", NA, NA, NA, NA, NA
      ),
      nrow = 2,
      ncol = 7,
      byrow = TRUE,
      dimnames = list(
        c("1", "2"),
        c("Package", "Version", "Priority", "Depends", "Imports", "LinkingTo", "Suggests")
      )
    )
    
    with_mocked_bindings(
      get_available_packages = function(repos) mock_matrix,
      .package = "val.meter",
      {
        p <- pkg(
          cran_repo_resource(
            package = "lonelypkg",
            repo = "https://cloud.r-project.org/"
          ),
          permissions = permissions("network")
        )
        
        deps <- p$cran_reverse_dependencies
        
        # Should be character, empty vector
        expect_type(deps, "character")
        expect_equal(length(deps), 0)
      }
    )
  })
  
  it("generates realistic mock data for random packages", {
    # Generate multiple random packages and check distribution
    n <- 50
    results <- replicate(n, {
      p <- random_pkg()
      p$cran_reverse_dependencies
    })
    
    # All should be character vectors
    expect_true(all(vlapply(results, is.character)))
    
    # Lengths should vary but be reasonable (0-10)
    lengths <- vapply(results, length, integer(1))
    expect_true(all(lengths >= 0))
    expect_true(all(lengths <= 10))
  })
})

describe("cran_reverse_dependencies_count metric behavior", {
  it("returns integer count matching dependency vector length", {
    mock_matrix <- create_mock_package_matrix()
    
    with_mocked_bindings(
      get_available_packages = function(repos) mock_matrix,
      .package = "val.meter",
      {
        p <- pkg(
          cran_repo_resource(
            package = "targetpkg",
            repo = "https://cloud.r-project.org/"
          ),
          permissions = permissions("network")
        )
        
        count <- p$cran_reverse_dependencies_count
        deps <- p$cran_reverse_dependencies
        
        expect_equal(count, length(deps))
        expect_type(count, "integer")
        expect_length(count, 1)
      }
    )
  })
})

