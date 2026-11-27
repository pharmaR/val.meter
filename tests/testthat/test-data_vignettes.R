cran_vignette_pkg <- function(html) {
  resource <- cran_repo_resource(
    repo = "https://cloud.r-project.org/",
    package = "fakepkg",
    version = "1.0.0"
  )
  p <- pkg(resource, permissions = "network")
  assign("web_html", xml2::read_html(html), envir = p@data)
  p
}

describe("vignette_count for cran_repo_resource", {
  it("returns an integer zero when there are no vignettes", {
    p <- cran_vignette_pkg(
      "<html><body><a href='/other'>other</a></body></html>"
    )

    expect_identical(p$vignette_count, 0L)
  })

  it("counts unique vignettes by file stem", {
    p <- cran_vignette_pkg(paste0(
      "<html><body>",
      "<a href='vignettes/intro.html'>intro</a>",
      "<a href='vignettes/intro.Rmd'>intro source</a>",
      "<a href='vignettes/advanced.html'>advanced</a>",
      "</body></html>"
    ))

    expect_identical(p$vignette_count, 2L)
  })
})
