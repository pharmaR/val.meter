describe("metrics()", {
  it("returns available metrics with no input", {
    m <- metrics()
    expect_true(length(m) > 0)
    expect_true(!is.null(names(m)))
    expect_true(all(vlapply(m, inherits, class_data_info)))
  })

  it("returns available metrics given a resource class", {
    m <- metrics(class_install_resource)
    expect_true(length(m) > 0)
    expect_true(!is.null(names(m)))
    expect_true(all(vlapply(m, inherits, class_data_info)))
  })

  it("calculates package metrics with a package object", {
    p <- random_pkg()
    m <- metrics(p)
    expect_true(length(m) > 0)
    expect_true(!is.null(names(m)))
    expect_true(all(vlapply(m, is.numeric)))
  })
})
