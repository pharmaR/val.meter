describe("news_version_forms", {
  it("drops trailing .0 components but keeps major.minor", {
    expect_identical(news_version_forms("1.2.0"), c("1.2.0", "1.2"))
    expect_identical(news_version_forms("2.0.0"), c("2.0.0", "2.0"))
    expect_identical(news_version_forms("1.2.1"), "1.2.1")
    expect_identical(news_version_forms("1.1.1.9000"), "1.1.1.9000")
  })
})

describe("news_mentions_version", {
  # the cases reported in #53
  it("treats a development version as matching a development heading", {
    news <- c("# pkg (development version)", "", "* new thing")
    expect_true(news_mentions_version(news, "1.1.1.9000"))
  })

  it("does not reduce 2.0.0 to a bare 2", {
    news <- "# pkg 1.4.2 released long ago"
    expect_false(news_mentions_version(news, "2.0.0"))
  })

  it("does not match a longer version", {
    expect_false(news_mentions_version("# pkg 1.2.10", "1.2.1"))
    expect_false(news_mentions_version("# pkg 11.2.1", "1.2.1"))
    expect_false(news_mentions_version("# pkg 1.2.1.1", "1.2.1"))
  })

  it("treats dots literally", {
    expect_false(news_mentions_version("# pkg 1a2b1", "1.2.1"))
  })

  # expected matches
  it("matches the version in common heading styles", {
    expect_true(news_mentions_version("# pkg 1.2.1", "1.2.1"))
    expect_true(news_mentions_version("pkg v1.2.1 (2026-01-01)", "1.2.1"))
    expect_true(news_mentions_version("CHANGES IN VERSION 1.2.1:", "1.2.1"))
    expect_true(news_mentions_version("# pkg 1.2.1.", "1.2.1"))
  })

  it("matches a shortened form of a .0 version", {
    expect_true(news_mentions_version("# pkg 2.0", "2.0.0"))
    expect_true(news_mentions_version("# pkg 1.2", "1.2.0"))
  })

  it("does not treat a release as a development version", {
    expect_false(news_mentions_version("# pkg (development version)", "1.2.1"))
  })

  it("searches a list of NEWS files", {
    news <- list(c("# pkg 1.0.0"), c("# pkg 1.2.1"))
    expect_true(news_mentions_version(news, "1.2.1"))
    expect_false(news_mentions_version(news, "1.3.0"))
  })
})

describe("has_current_news for a local package", {
  local_news_pkg <- function(version, news, env = parent.frame()) {
    path <- withr::local_tempdir(.local_envir = env)
    writeLines(
      c("Package: newspkg", paste("Version:", version)),
      file.path(path, "DESCRIPTION")
    )
    if (!is.null(news)) writeLines(news, file.path(path, "NEWS.md"))
    pkg(
      source_code_resource(package = "newspkg", version = version, path = path),
      permissions = permissions(TRUE)
    )
  }

  it("is TRUE when NEWS.md has the current version", {
    p <- local_news_pkg("1.2.1", c("# newspkg 1.2.1", "", "* fix"))
    expect_true(p$has_current_news)
  })

  it("is TRUE for a development version with a development heading", {
    p <- local_news_pkg("1.2.1.9000", "# newspkg (development version)")
    expect_true(p$has_current_news)
  })

  it("is FALSE when NEWS.md only has an older, longer-looking version", {
    p <- local_news_pkg("1.2.1", c("# newspkg 1.2.10", "", "* old"))
    expect_false(p$has_current_news)
  })

  it("is FALSE when there is no NEWS file", {
    p <- local_news_pkg("1.2.1", NULL)
    expect_false(p$has_current_news)
  })
})
