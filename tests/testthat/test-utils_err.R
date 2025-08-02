describe("cnd_type()", {
  it("produces expected condition classes", {
    expect_equal(
      cnd_type(),
      "val_meter_error"
    )
    
    expect_equal(
      cnd_type("test"),
      c("val_meter_test_error", "val_meter_error")
    )
    
    expect_equal(
      cnd_type("test", cnd = "warning"),
      c("val_meter_test_warning", "val_meter_warning")
    )
  })
})

describe("cnd_class_from_type", {
  it("deduces appropriate class from a type", {
    expect_equal(
      cnd_class_from_type("val_meter_test_error"),
      "test"
    )
    
    expect_equal(
      cnd_class_from_type("val_meter_error"),
      ""
    )
  })
})

describe("err", {
  it("exposes constructors which produce expected error classes", {
    for (n in names(err)) {
      # construct some dummy arguments to pass
      args <- as.list(setdiff(formalArgs(err[[n]]), "..."))
      names(args) <- args
      e <- tryCatch(do.call(err[[n]], args), error = identity)
      expect_s3_class(e, "error")
      expect_s3_class(e, cnd_type(n))
    }
  })
})

describe("error", {
  it("parses error codes", {
    e1 <- error("missing_suggests", suggests = "test")
    e2 <- tryCatch(err$missing_suggests(suggests = "test"), error = identity)
    expect_identical(e1$message, e2$message)
    expect_identical(class(e1), class(e2))
  })
})

describe("val.meter errors", {
  it("implements to_dcf", {
    e <- tryCatch(err$missing_suggests(suggests = "test"), error = identity)
    expect_identical(
      to_dcf(e),
      'val.meter::error("missing_suggests", suggests = "test")'
    )
  })
})