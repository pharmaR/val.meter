describe("discover_derive_deps", {
  it("discovers accessors of `pkg` and returns accessed names", {
    expr <- quote({
      if (TRUE) {
        # arbitrary nesting
        pkg$a
      }

      pkg$b + arbitrary_call(pkg$c)
    })

    expect_equal(
      sort(discover_data_derive_deps(expr)),
      c("a", "b", "c")
    )
  })
})
