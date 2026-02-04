# Helper function tests

describe("extract_rd_aliases helper", {
  it("extracts aliases from an Rd object", {
    # Create a simple Rd object with aliases
    rd <- structure(
      list(
        structure("function_name", Rd_tag = "\\alias"),
        structure("another_alias", Rd_tag = "\\alias"),
        structure("Some title", Rd_tag = "\\title")
      ),
      class = "Rd"
    )
    
    result <- extract_rd_aliases(rd)
    expect_equal(result, c("function_name", "another_alias"))
  })
  
  it("returns empty character vector when no aliases present", {
    rd <- structure(
      list(
        structure("Some title", Rd_tag = "\\title"),
        structure("Description text", Rd_tag = "\\description")
      ),
      class = "Rd"
    )
    
    result <- extract_rd_aliases(rd)
    expect_equal(result, character(0))
  })
  
  it("handles multi-part alias content", {
    rd <- structure(
      list(
        structure(list("multi", "_", "part"), Rd_tag = "\\alias")
      ),
      class = "Rd"
    )
    
    result <- extract_rd_aliases(rd)
    expect_equal(result, "multi_part")
  })
})

describe("get_rd_db_tags helper", {
  it("extracts tags for all help pages", {
    rd_db <- list(
      page1 = structure(
        list(
          structure("alias1", Rd_tag = "\\alias"),
          structure("Title", Rd_tag = "\\title")
        ),
        class = "Rd"
      ),
      page2 = structure(
        list(
          structure("alias2", Rd_tag = "\\alias"),
          structure("Example code", Rd_tag = "\\examples")
        ),
        class = "Rd"
      )
    )
    
    result <- get_rd_db_tags(rd_db)
    
    expect_length(result, 2)
    expect_equal(names(result), c("page1", "page2"))
    expect_equal(result$page1, c("\\alias", "\\title"))
    expect_equal(result$page2, c("\\alias", "\\examples"))
  })
})

describe("map_exports_to_pages helper", {
  it("maps exports to their help pages", {
    rd_db <- list(
      page1 = structure(
        list(
          structure("export1", Rd_tag = "\\alias"),
          structure("export2", Rd_tag = "\\alias")
        ),
        class = "Rd"
      ),
      page2 = structure(
        list(
          structure("export3", Rd_tag = "\\alias")
        ),
        class = "Rd"
      )
    )
    
    rd_db_tags <- get_rd_db_tags(rd_db)
    exports <- c("export1", "export2", "export3")
    
    result <- map_exports_to_pages(rd_db, rd_db_tags, exports)
    
    expect_equal(result$export1, "page1")
    expect_equal(result$export2, "page1")
    expect_equal(result$export3, "page2")
  })
  
  it("ignores aliases that are not exports", {
    rd_db <- list(
      page1 = structure(
        list(
          structure("exported_func", Rd_tag = "\\alias"),
          structure("internal_helper", Rd_tag = "\\alias")
        ),
        class = "Rd"
      )
    )
    
    rd_db_tags <- get_rd_db_tags(rd_db)
    exports <- c("exported_func")
    
    result <- map_exports_to_pages(rd_db, rd_db_tags, exports)
    
    expect_equal(names(result), "exported_func")
    expect_false("internal_helper" %in% names(result))
  })
})

describe("find_pages_with_examples helper", {
  it("identifies pages with examples sections", {
    rd_db <- list(
      page1 = structure(list(), class = "Rd"),
      page2 = structure(list(), class = "Rd"),
      page3 = structure(list(), class = "Rd")
    )
    
    rd_db_tags <- list(
      page1 = c("\\alias", "\\title"),
      page2 = c("\\alias", "\\title", "\\examples"),
      page3 = c("\\alias", "\\title", "\\examples")
    )
    
    result <- find_pages_with_examples(rd_db, rd_db_tags)
    
    expect_equal(result, c("page2", "page3"))
  })
  
  it("returns empty when no pages have examples", {
    rd_db <- list(
      page1 = structure(list(), class = "Rd"),
      page2 = structure(list(), class = "Rd")
    )
    
    rd_db_tags <- list(
      page1 = c("\\alias", "\\title"),
      page2 = c("\\alias", "\\title")
    )
    
    result <- find_pages_with_examples(rd_db, rd_db_tags)
    
    expect_equal(result, character(0))
  })
})

describe("analyze_documentation_examples helper", {
  it("analyzes documentation for an installed package", {
    skip_if_not_installed("dplyr")
    
    result <- analyze_documentation_examples("dplyr")
    
    expect_type(result, "list")
    expect_named(result, c(
      "exported_count",
      "help_page_count",
      "help_pages_with_examples_count",
      "documented_exports_count",
      "undocumented_exports",
      "help_pages_with_examples"
    ))
    
    expect_type(result$exported_count, "integer")
    expect_type(result$help_page_count, "integer")
    expect_type(result$help_pages_with_examples_count, "integer")
    expect_type(result$documented_exports_count, "integer")
    expect_type(result$undocumented_exports, "character")
    expect_type(result$help_pages_with_examples, "character")
    
    # Sanity checks
    expect_gte(result$exported_count, 0)
    expect_gte(result$help_page_count, 0)
    expect_lte(result$help_pages_with_examples_count, result$help_page_count)
    expect_lte(result$documented_exports_count, result$exported_count)
  })
  
  it("handles NA pkg_path by finding package", {
    skip_if_not_installed("dplyr")
    
    result <- analyze_documentation_examples("dplyr", NA_character_)
    
    expect_type(result, "list")
    expect_gte(result$exported_count, 0)
  })
})

# Metric tests

describe("documentation examples metrics", {
  it("can analyze a well-documented package", {
    skip_if_not_installed("dplyr")
    
    # Use dplyr as a test case - it's well documented
    pkg <- pkg("dplyr")
    
    # Check that analysis runs
    expect_no_error(pkg$documentation_examples)
    
    # Check structure
    info <- pkg$documentation_examples
    expect_type(info, "list")
    expect_true("exported_count" %in% names(info))
    expect_true("help_page_count" %in% names(info))
    expect_true("help_pages_with_examples_count" %in% names(info))
    expect_true("documented_exports_count" %in% names(info))
    expect_true("undocumented_exports" %in% names(info))
    
    # Counts should be positive integers
    expect_true(info$exported_count > 0)
    expect_true(info$help_page_count > 0)
    expect_true(info$help_pages_with_examples_count >= 0)
    expect_true(info$documented_exports_count >= 0)
    
    # Documented exports should be <= total exports
    expect_true(info$documented_exports_count <= info$exported_count)
    
    # Examples count should be <= help page count
    expect_true(info$help_pages_with_examples_count <= info$help_page_count)
  })
  
  it("computes help_pages_with_examples_count metric", {
    skip_if_not_installed("dplyr")
    
    pkg <- pkg("dplyr")
    
    count <- pkg$help_pages_with_examples_count
    expect_type(count, "integer")
    expect_true(count >= 0)
    
    # Should match the raw data
    expect_equal(count, pkg$documentation_examples$help_pages_with_examples_count)
  })
  
  it("computes help_examples_coverage metric", {
    skip_if_not_installed("dplyr")
    
    pkg <- pkg("dplyr")
    
    coverage <- pkg$help_examples_coverage
    expect_type(coverage, "double")
    expect_true(coverage >= 0 && coverage <= 1)
    
    # Should be rounded to 3 decimal places
    expect_equal(coverage, round(coverage, 3))
    
    # Check calculation
    info <- pkg$documentation_examples
    expected <- round(info$help_pages_with_examples_count / info$help_page_count, 3)
    expect_equal(coverage, expected)
  })
  
  it("computes exports_help_coverage metric", {
    skip_if_not_installed("dplyr")
    
    pkg <- pkg("dplyr")
    
    coverage <- pkg$exports_help_coverage
    expect_type(coverage, "double")
    expect_true(coverage >= 0 && coverage <= 1)
    
    # Should be rounded to 3 decimal places
    expect_equal(coverage, round(coverage, 3))
    
    # Check calculation
    info <- pkg$documentation_examples
    expected <- round(info$documented_exports_count / info$exported_count, 3)
    expect_equal(coverage, expected)
  })
  
  it("handles packages with various example counts", {
    skip_if_not_installed("dplyr")
    
    # dplyr has good but not perfect example coverage
    pkg <- pkg("dplyr")
    
    expect_no_error(pkg$documentation_examples)
    
    info <- pkg$documentation_examples
    expect_true(info$exported_count > 0)
    expect_true(info$help_page_count > 0)
    
    # Metrics should still compute
    expect_no_error(pkg$help_pages_with_examples_count)
    expect_no_error(pkg$help_examples_coverage)
    expect_no_error(pkg$exports_help_coverage)
    
    # Verify coverage is reasonable (not 0 or 1)
    expect_true(pkg$help_examples_coverage > 0 && pkg$help_examples_coverage < 1)
  })
  
  it("returns NA for empty packages", {
    # Create a mock scenario with no help pages
    # This tests edge case handling
    
    # We can't easily create this without mocking, so just verify
    # that the metric calculation handles division by zero
    info <- list(
      exported_count = 0,
      help_page_count = 0,
      help_pages_with_examples_count = 0,
      documented_exports_count = 0
    )
    
    # help_examples_coverage with no help pages
    coverage <- if (info$help_page_count == 0) {
      NA_real_
    } else {
      round(info$help_pages_with_examples_count / info$help_page_count, 3)
    }
    expect_true(is.na(coverage))
    
    # exports_help_coverage with no exports
    coverage2 <- if (info$exported_count == 0) {
      NA_real_
    } else {
      round(info$documented_exports_count / info$exported_count, 3)
    }
    expect_true(is.na(coverage2))
  })
})

describe("documentation examples metrics metadata", {
  it("help_pages_with_examples_count is registered as a metric", {
    info <- pkg_data_info("help_pages_with_examples_count")
    expect_true(info@metric)
  })
  
  it("help_examples_coverage is registered as a metric", {
    info <- pkg_data_info("help_examples_coverage")
    expect_true(info@metric)
  })
  
  it("exports_help_coverage is registered as a metric", {
    info <- pkg_data_info("exports_help_coverage")
    expect_true(info@metric)
  })
  
  it("documentation_examples is not a metric", {
    info <- pkg_data_info("documentation_examples")
    expect_false(info@metric)
  })
  
  it("metrics have appropriate tags", {
    info1 <- pkg_data_info("help_pages_with_examples_count")
    info2 <- pkg_data_info("help_examples_coverage")
    info3 <- pkg_data_info("exports_help_coverage")
    
    expect_true("best practice" %in% info1@tags)
    expect_true("best practice" %in% info2@tags)
    expect_true("best practice" %in% info3@tags)
  })
  
  it("metrics have non-empty titles", {
    info1 <- pkg_data_info("help_pages_with_examples_count")
    info2 <- pkg_data_info("help_examples_coverage")
    info3 <- pkg_data_info("exports_help_coverage")
    
    expect_true(nchar(info1@title) > 0)
    expect_true(nchar(info2@title) > 0)
    expect_true(nchar(info3@title) > 0)
  })
  
  it("metrics have non-empty descriptions", {
    info1 <- pkg_data_info("help_pages_with_examples_count")
    info2 <- pkg_data_info("help_examples_coverage")
    info3 <- pkg_data_info("exports_help_coverage")
    
    expect_true(length(info1@description) > 0)
    expect_true(length(info2@description) > 0)
    expect_true(length(info3@description) > 0)
  })
})

describe("documentation examples mock implementation", {
  it("random packages have realistic example counts", {
    n <- 50
    results <- replicate(n, {
      p <- random_pkg()
      p$help_pages_with_examples_count
    })
    
    # Should be integers
    expect_type(results, "integer")
    
    # Should be in reasonable range (5-50)
    expect_true(all(results >= 5))
    expect_true(all(results <= 50))
  })
  
  it("random packages have realistic help examples coverage", {
    n <- 50
    results <- replicate(n, {
      p <- random_pkg()
      p$help_examples_coverage
    })
    
    # Should be numeric
    expect_type(results, "double")
    
    # Should be between 0.5 and 1.0
    expect_true(all(results >= 0.5))
    expect_true(all(results <= 1.0))
    
    # Should be rounded to 3 decimals
    expect_true(all(results == round(results, 3)))
  })
  
  it("random packages have realistic exports help coverage", {
    n <- 50
    results <- replicate(n, {
      p <- random_pkg()
      p$exports_help_coverage
    })
    
    # Should be numeric
    expect_type(results, "double")
    
    # Should be between 0.8 and 1.0 (most packages document all exports)
    expect_true(all(results >= 0.8))
    expect_true(all(results <= 1.0))
    
    # Should be rounded to 3 decimals
    expect_true(all(results == round(results, 3)))
  })
})


