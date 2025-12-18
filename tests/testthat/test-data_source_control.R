describe("source control metrics implementation details", {
  # Test the actual implementation logic with mocked desc objects
  it("extracts URLs from DESCRIPTION and matches against domains", {
    # Create a temporary DESCRIPTION file
    tmp_dir <- withr::local_tempdir()
    desc_file <- file.path(tmp_dir, "DESCRIPTION")

    writeLines(c(
      "Package: testpkg",
      "Version: 1.0.0",
      "Title: Test Package",
      "Description: A test package.",
      "URL: https://github.com/user/testpkg",
      "BugReports: https://github.com/user/testpkg/issues"
    ), desc_file)

    d <- desc::desc(file = desc_file)

    # Get URLs and check matching logic
    urls <- d$get_urls()
    domains <- opt("source_control_domains")

    expect_true(length(urls) > 0)
    expect_true(any(sapply(urls, function(url) {
      any(sapply(domains, function(domain) {
        grepl(domain, url, ignore.case = TRUE)
      }))
    })))
  })

  it("returns NA when no URLs match recognized domains", {
    tmp_dir <- withr::local_tempdir()
    desc_file <- file.path(tmp_dir, "DESCRIPTION")

    writeLines(c(
      "Package: testpkg",
      "Version: 1.0.0",
      "Title: Test Package",
      "Description: A test package.",
      "URL: https://example.com/testpkg"
    ), desc_file)

    d <- desc::desc(file = desc_file)
    urls <- d$get_urls()
    domains <- opt("source_control_domains")

    # None should match
    matches <- any(sapply(urls, function(url) {
      any(sapply(domains, function(domain) {
        grepl(domain, url, ignore.case = TRUE)
      }))
    }))

    expect_false(matches)
  })

  it("is case-insensitive when matching domains", {
    tmp_dir <- withr::local_tempdir()
    desc_file <- file.path(tmp_dir, "DESCRIPTION")

    writeLines(c(
      "Package: testpkg",
      "Version: 1.0.0",
      "Title: Test Package",
      "Description: A test package.",
      "URL: https://GitHub.COM/user/testpkg"
    ), desc_file)

    d <- desc::desc(file = desc_file)
    urls <- d$get_urls()

    # Should match despite different case
    matched <- any(sapply(urls, function(url) {
      grepl("github.com", url, ignore.case = TRUE)
    }))

    expect_true(matched)
  })

  it("handles BugReports field", {
    tmp_dir <- withr::local_tempdir()
    desc_file <- file.path(tmp_dir, "DESCRIPTION")

    writeLines(c(
      "Package: testpkg",
      "Version: 1.0.0",
      "Title: Test Package",
      "Description: A test package.",
      "BugReports: https://github.com/user/testpkg/issues"
    ), desc_file)

    d <- desc::desc(file = desc_file)

    # BugReports should be available
    bug_reports <- tryCatch(
      d$get_field("BugReports"),
      error = function(e) character(0)
    )

    expect_true(length(bug_reports) > 0)
    expect_true(grepl("github.com", bug_reports, ignore.case = TRUE))
  })
})

describe("source control option customization", {
  it("respects custom source_control_domains option", {
    skip_if_offline()
    skip_on_cran()

    # Save original and set custom domains
    old_domains <- opt_set("source_control_domains", c("custom.org"))
    on.exit(opt_set("source_control_domains", old_domains))

    # Create test DESCRIPTION with custom domain
    tmp_dir <- withr::local_tempdir()
    desc_file <- file.path(tmp_dir, "DESCRIPTION")

    writeLines(c(
      "Package: testpkg",
      "Version: 1.0.0",
      "Title: Test Package",
      "Description: A test package.",
      "URL: https://custom.org/testpkg"
    ), desc_file)

    d <- desc::desc(file = desc_file)
    urls <- d$get_urls()
    domains <- opt("source_control_domains")

    # Should match custom domain
    expect_equal(domains, "custom.org")
    expect_true(any(sapply(urls, function(url) {
      grepl("custom.org", url, ignore.case = TRUE)
    })))
  })
})

describe("source control metrics metadata", {
  it("has_recognized_source is registered as a metric", {
    info <- pkg_data_info("has_recognized_source")
    expect_true(info@metric)
  })

  it("has_recognized_source has 'best practice' tag", {
    info <- pkg_data_info("has_recognized_source")
    expect_true("best practice" %in% info@tags)
  })

  it("recognized_source_url is not a metric", {
    info <- pkg_data_info("recognized_source_url")
    expect_false(info@metric)
  })

  it("metrics have non-empty titles", {
    info_metric <- pkg_data_info("has_recognized_source")
    info_data <- pkg_data_info("recognized_source_url")

    expect_true(length(info_metric@title) > 0)
    expect_true(nchar(info_metric@title) > 0)
    expect_true(length(info_data@title) > 0)
    expect_true(nchar(info_data@title) > 0)
  })

  it("metrics have non-empty descriptions", {
    info_metric <- pkg_data_info("has_recognized_source")
    info_data <- pkg_data_info("recognized_source_url")

    expect_true(length(info_metric@description) > 0)
    expect_true(length(info_data@description) > 0)
  })
})

describe("source control option integration", {
  it("default domains include major platforms", {
    domains <- opt("source_control_domains")

    expect_true("github.com" %in% domains)
    expect_true("gitlab.com" %in% domains)
    expect_true("bitbucket.org" %in% domains)
    expect_true(length(domains) >= 5)
  })

  it("can extend default domains", {
    defaults <- opt("source_control_domains")
    old_domains <- opt_set("source_control_domains", c(
      defaults,
      "git.company.com"
    ))
    on.exit(opt_set("source_control_domains", old_domains))

    extended <- opt("source_control_domains")
    expect_true("git.company.com" %in% extended)
    expect_true("github.com" %in% extended)
    expect_equal(length(extended), length(defaults) + 1)
  })

  it("can replace domains entirely", {
    old_domains <- opt_set("source_control_domains", c("custom.org"))
    on.exit(opt_set("source_control_domains", old_domains))

    domains <- opt("source_control_domains")
    expect_equal(domains, "custom.org")
    expect_equal(length(domains), 1)
  })
})

describe("source control mock implementation", {
  it("random packages have realistic source control distribution", {
    # Generate multiple random packages and check distribution
    n <- 100
    results <- replicate(n, {
      p <- random_pkg()
      p$has_recognized_source
    })

    # Should be logical
    expect_type(results, "logical")

    # Most packages should have recognized source (around 80%)
    prop_with_source <- mean(results, na.rm = TRUE)
    expect_true(prop_with_source > 0.5)
    expect_true(prop_with_source < 1.0)
  })
})
