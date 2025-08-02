describe("impl_data", {
  it("can overwrite existing implementations when overwrite=TRUE", {
    expect_silent({
      impl_data(
        "name_character_count",
        overwrite = TRUE,
        quiet = TRUE,
        function(pkg, ...) {
          nchar(pkg$name)
        }
      )
    })
  })
  
  it("raises a message when overwriting by default", {
    suppressMessages({
      expect_message(regexp = "[oO]verwriting", {
        impl_data(
          "name_character_count",
          overwrite = TRUE,
          function(pkg, ...) {
            nchar(pkg$name)
          }
        )
      })
    })
  })

  it("is silent when overwriting if explicitly made quiet", {
    expect_silent({
      impl_data(
        "name_character_count",
        overwrite = TRUE,
        quiet = TRUE,
        function(pkg, ...) {
          nchar(pkg$name)
        }
      )
    })
  })
  
  it("registers data info when executed", {
    expect_silent({
      impl_data(
        "name_character_count",
        overwrite = TRUE,
        quiet = TRUE,
        function(pkg, ...) {
          nchar(pkg$name)
        }
      )
    })
    
    it("is a non-metric by default", {
      info <- pkg_data_info("name_character_count")
      expect_false(info@metric)
    })
    
    it("initializes with empty title and description", {
      info <- pkg_data_info("name_character_count")
      expect_identical(info@title, character(0L))
      expect_identical(info@description, rd_empty())
    })
    
    it("initializes with empty tags, permissions and suggests", {
      info <- pkg_data_info("name_character_count")
      expect_identical(info@tags, tags(character(0L)))
      expect_identical(info@permissions, permissions(character(0L)))
      expect_identical(info@suggests, character(0L))
    })
  })
  
  it("registers metric info with enforced metric policies", {
    expect_error(regexp = "atomic", {
      impl_data(
        "name_character_count",
        metric = TRUE,
        overwrite = TRUE,
        quiet = TRUE,
        function(pkg, ...) {
          nchar(pkg$name)
        }
      )
    })
  })
  
  it("produces a new field for package data", {
    p <- pkg(mock_resource(package = "test", version = "1.2.3"))
    expect_equal(
      p$name_character_count,
      nchar("test")
    )
  })
})