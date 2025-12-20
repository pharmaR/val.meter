test_that("logs are captured during package data evaluation", {
  impl_data(
    "logs_test_name_character_count",
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      cat("text\n")
      message("message")
      warning("warning")
      cli::cat_line(cli::col_blue("blue"))
      nchar(pkg$name)
    }
  )

  # expect that we have produced some logs
  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  expect_no_error(p$logs_test_name_character_count)
  expect_true(!is.null(logs <- p@logs[["logs_test_name_character_count"]]))

  # expect to find our output in our logs
  expect_true(is.character(text_logs <- format(logs, style = "text")))
  expect_match(text_logs, "text")
  expect_match(text_logs, "\\bmessage\\b")
  expect_match(text_logs, "\\bWarning in")
  expect_match(text_logs, "\\bwarning\\b")

  # expect that we can format our logs as ansi strings
  expect_true(is.character(ansi_logs <<- format(logs, style = "ansi")))
  expect_true(nchar(ansi_logs) > nchar(text_logs))

  # expect that we can produce an html div from our logs
  expect_s3_class(html_logs <- format(logs, style = "html"), "shiny.tag")
})
